package tgs;

import static com.google.common.truth.Truth.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;
import java.util.Arrays;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;

public class ASTTest {

  private static ImmutableMap<String, AST> parseAST(String content) throws CompilerException {
    ImmutableList<Tokenizer.TagOrText> tokens = new Tokenizer("/test/file.txt", content).tokenize();

    SublimeSyntaxCompiler.Input.Builder syntaxInputBuilder = SublimeSyntaxCompiler.Input.builder();
    tokens.forEach(syntaxInputBuilder::addTagOrText);
    try {
      syntaxInputBuilder.build();
    } catch (CompilerException ignore) {
      // Ignore.
    } catch (Exception ex) {
      Assertions.fail("Unexpected non-CompilerException: " + ex.getMessage());
    }

    AST.Builder builder = new AST.Builder();
    for (Tokenizer.TagOrText tagOrText : tokens) {
      if (tagOrText.isTag()) {
        builder.consumeTag(TypedTag.parse(tagOrText.tag()));
      } else if (tagOrText.isWhitespace()) {
        builder.consumeWhitespace(tagOrText.whitespace());
      } else {
        builder.consumeText(tagOrText.text());
      }
    }
    AST ast = builder.build();
    return ImmutableMap.of("/test/file.txt", ast);
  }

  private static void testSyntaxGen(String content) throws IOException, CompilerException {
    SublimeSyntaxCompiler.Input.Builder inputBuilder = SublimeSyntaxCompiler.Input.builder();
    new Tokenizer("/test/file.txt", content).tokenize().forEach(inputBuilder::addTagOrText);

    new SublimeSyntaxCompiler(inputBuilder.build()).compileRules();
  }

  private static void parse(String content) throws CompilerException {
    ImmutableMap<String, AST> asts = parseAST(content);
    ASTValidator validator = new ASTValidator(asts);
    ImmutableList<CompilerException> errors = validator.computeErrors();
    if (!errors.isEmpty()) throw errors.get(0);

    // Check that compilation works.
    Compiler compiler = new Compiler(asts.values(), validator.labelRegistry());
    compiler.compile();
  }

  private static void assertParses(String... lines) throws CompilerException {
    parse(Arrays.asList(lines).stream().collect(Collectors.joining("\n")));
  }

  private static void assertErrors(String errorSubstr, String... lines) throws CompilerException {
    CompilerException ex =
        assertThrows(
            CompilerException.class,
            () -> parse(Arrays.asList(lines).stream().collect(Collectors.joining("\n"))));
    assertThat(ex).hasMessageThat().contains(errorSubstr);
  }

  @Test
  public void syntaxGen() throws CompilerException, IOException {
    testSyntaxGen("");
    testSyntaxGen("[define_enum: Foo BAR BAZ]");
  }

  @Test
  public void basicBlocks() throws CompilerException {
    assertParses("// empty file");
    assertParses("[block] with text");
    assertParses("[block] /* with no text */ [no_text]");
    assertParses("[block: with_id] and text");

    assertErrors("text not associated", "text without block");
    assertErrors("[no_text] wasn't specified", "[block] // with no text");
    assertErrors("unexpected argument", "[block: with multiple ids] and text");
  }

  @Test
  public void blockIds() throws CompilerException {
    assertParses("[block: with_id] and text");
    assertParses("[extern_block: with_id]");
    assertParses("[block: id1] text1", "[block: id2] text two");
    assertParses("[block] going [to: id]", "[block] unreachable", "[block: id] destination");
    assertParses(
        "[extern_script: $script(num:INTEGER):BLOCK_ID]", "[block] going [to: $script(123)]");

    assertErrors("illegal character", "[block: #illegal.id] text");
    assertErrors("text not associated with a [block] or [prompt]", "[extern_block: foo] text");
    assertErrors("duplicate block id", "[block: duplicate_id] text", "[block: duplicate_id] text2");
    assertErrors("duplicate block id", "[extern_block: whoo]", "[block: whoo] text");
    assertErrors(
        "found duplicate",
        "[block: id1] texty",
        "[block: id2] texty",
        "[block] with [to: id1] two destinations [to: id2]");
    assertErrors("undefined block id", "[block] text [to: nonexistent]");
    assertErrors(
        "undefined block id",
        "[prompt: pid] fooey",
        "[option] option one [to: prompt_dest]",
        "[option] option two [to: prompt_dest]",
        "[block: prompt_dest] texty [to: pid]");
  }

  @Test
  public void blockExpression() throws CompilerException {
    assertParses("[define: foo BLOCK_ID.id1]", "[block: id1] text");
    assertParses(
        "[extern_script: $foo(id:BLOCK_ID):VOID]", "[block: id1] text [do: $foo(BLOCK_ID.id1)]");
    assertParses(
        "[define: foo BLOCK_ID.id1]",
        "[extern_script: $sfoo(id:BLOCK_ID):VOID]",
        "[block: id1] text [do: $sfoo(foo)]");

    assertErrors("Block id 'doesnotexist'", "[define: foo BLOCK_ID.doesnotexist]");
    assertErrors(
        "Block id 'doesnotexist'",
        "[extern_script: $foo(id:BLOCK_ID):VOID]",
        "[block] text [do: $foo(BLOCK_ID.doesnotexist)]");
  }

  @Test
  public void opSplitting() throws CompilerException {
    assertParses("[define: aa 1]", "[define: bb 2]", "[block] [if: aa+2>bb] foo [end_if]");
    assertParses("[define: aa 1]", "[define: bb 2]", "[block] [if: aa>=bb-1] foo [end_if]");
    assertParses("[define: aa 1]", "[define: bb 2]", "[block] [do: aa++] [no_text]");
    assertParses("[define: aa 1]", "[define: bb 2]", "[block] [do: bb+=3] [no_text]");
    assertParses("[define: aa -1]", "[define: bb -2]", "[block] [if: -aa > bb] foo [end_if]");
    assertParses("[define: aa -1]", "[define: bb -2]", "[block] [if: aa<-bb] foo [end_if]");
    assertParses("[define: aa -1]", "[define: bb -2]", "[block] [do: aa = 2+-bb] [no_text]");

    assertErrors("unexpected token", "[block] [if: 3- > 4] foo [end_if]");
    assertErrors(
        "Expected numeric type",
        "[define_enum: Weather SUNNY RAINY]",
        "[block] [if: -Weather.SUNNY] foo [end_if]");
    assertErrors(
        "unexpected token",
        "[define_enum: Weather SUNNY RAINY]",
        "[block] [if: Weather.-SUNNY] foo [end_if]");
  }

  @Test
  public void blockText() throws CompilerException {
    assertParses("[block] [no_text]");
    assertParses("[block] with text");
    assertParses("[block] [if: true] maybe text [else] [no_text] [end_if]");

    assertErrors("[no_text] wasn't specified", "[block] // no text");
    assertErrors("found [no_text] and text in the same [block]", "[block] text [no_text]");
    assertErrors("found duplicate [no_text]", "[block] [no_text] [no_text]");
    assertErrors(
        "found duplicate [no_text]",
        "[extern_script: $foo():BOOLEAN]",
        "[block]",
        "[if: $foo()] text [else] [no_text] [end_if]",
        "[if: $foo()] [no_text] [else] huh? [end_if]");
  }

  @Test
  public void basicSwitch() throws CompilerException {
    assertParses(
        "[define_enum: Weather SUNNY CLOUDY RAINY]",
        "[define: foo Weather.SUNNY]",
        "[block] [switch: foo] [case: SUNNY] Yay! [case: CLOUDY RAINY] Boo! [end_switch]");
    assertParses(
        "[define_enum: Weather SUNNY CLOUDY RAINY]",
        "[define: foo Weather.SUNNY]",
        "[block] [switch: foo] [case: SUNNY] Yay! [default] Boo! [end_switch]");

    assertErrors(
        "expected enum value here, but was INTEGER",
        "[define_enum: Weather SUNNY CLOUDY RAINY]",
        "[define: foo Weather.SUNNY]",
        "[block] [switch: 12 + 34] [case: SUNNY] Yay! [case: CLOUDY RAINY] Boo! [end_switch]");
    assertErrors(
        "for values: CLOUDY",
        "[define_enum: Weather SUNNY CLOUDY RAINY]",
        "[define: foo Weather.SUNNY]",
        "[block] [switch: foo] [case: SUNNY] Yay! [case: RAINY] Boo! [end_switch]");
    assertErrors(
        "Not a valid ENUM.Weather value",
        "[define_enum: Weather SUNNY CLOUDY RAINY]",
        "[define: foo Weather.SUNNY]",
        "[block] [switch: foo] [case: SUNNY SNOWY] Yay! [case: RAINY CLOUDY] Boo! [end_switch]");
    assertErrors(
        "Duplicate enum case",
        "[define_enum: Weather SUNNY CLOUDY RAINY]",
        "[define: foo Weather.SUNNY]",
        "[block] [switch: foo] [case: SUNNY CLOUDY] Yay! [case: CLOUDY RAINY] Boo! [end_switch]");
    assertErrors(
        "[default] case is inaccessible",
        "[define_enum: Weather SUNNY CLOUDY RAINY]",
        "[define: foo Weather.SUNNY]",
        "[block] [switch: foo] [case: SUNNY CLOUDY RAINY] Yay! [default] Boo! [end_switch]");
  }

  @Test
  public void subBlock() throws CompilerException {
    assertParses("[sub_block: foo] This is a thing!");
    assertParses("[extern_script: $foo():VOID]", "[sub_block: foo] This is a thing! [do: $foo()]");

    assertErrors("Expected argument", "[sub_block] This is a thing!");
    assertErrors(
        "[to] is not allowed in sub-blocks",
        "[sub_block: foo] This is a thing! [to: id]",
        "[block: id] This is the main thing!");
    assertErrors("sub_block 'foo' is fully recursive", "[sub_block: foo] foo [use_sub_block: foo]");
    assertErrors(
        "sub_block 'foo' is fully recursive",
        "[sub_block: foo] foo [use_sub_block: bar]",
        "[sub_block: bar] bar [use_sub_block: foo]");
  }

  @Test
  public void promptOptions() throws CompilerException {
    assertParses(
        "[block: dest] text",
        "[prompt: id] prompt text",
        "[option] one [to: dest]",
        "[option] two [to: dest]");
    assertParses(
        "[define: vari true]",
        "[block: dest] text",
        "[prompt: id] prompt text",
        "[option_if: vari] one [to: dest]",
        "[option] two [to: dest]");
    assertParses(
        "[define: vari true]",
        "[block: dest] text",
        "[prompt: id] prompt text",
        "[option] one [to: dest]",
        "[option_if: vari] two [to: dest]");
    assertParses(
        "[define: vari true]",
        "[block: dest] text",
        "[prompt: id] prompt text",
        "[option] one [to: dest]",
        "[option_if: vari] two [to: dest]");
    assertParses(
        "[define: vari true]",
        "[extern_script: $wherever():BLOCK_ID]",
        "[block: dest] text",
        "[prompt: id] prompt text",
        "[option_if: vari] one [to: dest]",
        "[option_if: vari] two [to: dest]",
        "[option_none] [to: dest]");

    assertErrors(
        "outside of a block or prompt",
        "[prompt] text",
        "[option] one[to: dest]",
        "[option] two[to: dest]");
    assertErrors("[option] tags require [to:", "[prompt: id] text", "[option] one", "[option] two");
    assertErrors("[prompt] has only one option", "[prompt: id] text", "[option] one [to: dest]");
    assertErrors(
        "[prompt] will only ever display exactly one option",
        "[prompt: id] text",
        "[option_if: $foo()] one [to: dest]",
        "[option_default] two [to: dest]");
  }

  @Test
  public void promptGroups() throws CompilerException {
    assertParses(
        "[extern_script: $whee():BLOCK_ID]",
        "[prompt: id] [no_text]",
        "[option_group] short",
        "[option] ha [to: $whee()]",
        "[option] ba [to: $whee()]",
        "[end_group]",
        "[option_group] long",
        "[option] haaaa [to: $whee()]",
        "[option] baaaa [to: $whee()]",
        "[end_group]");
    assertParses(
        "[extern_script: $ho():BOOLEAN]",
        "[extern_script: $ha():BLOCK_ID]",
        "[prompt: id] [no_text]",
        "[option_group] maybe empty",
        "[option_if: $ho()] whee [to: $ha()]",
        "[option_if: $ho()] whoa [to: $ha()]",
        "[end_group]",
        "[option_group] maybe empty two",
        "[option_if: $ho()] whup [to: $ha()]",
        "[option_if: $ho()] whazzam [to: $ha()]",
        "[end_group]",
        "[option_default] allowed [to: $ha()]");
    assertParses(
        "[extern_script: $ho():BOOLEAN]",
        "[extern_script: $ha():BLOCK_ID]",
        "[prompt: id] [no_text]",
        "[option_group] maybe empty",
        "[option_if: $ho()] whee [to: $ha()]",
        "[option_if: $ho()] whoa [to: $ha()]",
        "[option_default] whoaza [to: $ha()]",
        "[end_group]",
        "[option_group] maybe empty two",
        "[option_if: $ho()] whup [to: $ha()]",
        "[option_if: $ho()] whazzam [to: $ha()]",
        "[end_group]");
    assertParses(
        "[extern_script: $ho():BOOLEAN]",
        "[extern_script: $ha():BLOCK_ID]",
        "[prompt: id] [no_text]",
        "[option_group] nested",
        "[option_group] maybe empty",
        "[option_if: $ho()] whee [to: $ha()]",
        "[option_if: $ho()] whoa [to: $ha()]",
        "[end_group]",
        "[option_group_if: $ho()] maybe empty two",
        "[option_if: $ho()] whup [to: $ha()]",
        "[option_if: $ho()] whazzam [to: $ha()]",
        "[end_group]",
        "[end_group]",
        "[option_if: $ho()] top-level [to: $ha()]",
        "[option_default] allowed [to: $ha()]");

    assertErrors(
        "missing [end_group] for this [option_group]",
        "[extern_script: $whee():BLOCK_ID]",
        "[prompt: id] [no_text]",
        "[option_group] whee",
        "[option] hi [to: $whee()]",
        "[option] bye [to: $whee()]");
    assertErrors(
        "found text in a [prompt], but without an [option] or [option_group]",
        "[extern_script: $whee():BLOCK_ID]",
        "[prompt: id] [no_text]",
        "[option_group] whee",
        "[option] hi [to: $whee()]",
        "[option] bye [to: $whee()]",
        "[end_group]",
        "text");
    assertErrors(
        "[option_default] will never be shown",
        "[extern_script: $whee():BLOCK_ID]",
        "[prompt: id] [no_text]",
        "[option_group] short",
        "[option] ha [to: $whee()]",
        "[option] ba [to: $whee()]",
        "[end_group]",
        "[option_group] long",
        "[option] haaaa [to: $whee()]",
        "[option] baaaa [to: $whee()]",
        "[end_group]",
        "[option_default] whaaaaa [to: $whee()]");
    assertErrors(
        "[prompt] might display zero options",
        "[extern_script: $whee():BLOCK_ID]",
        "[prompt: id] [no_text]",
        "[option_group_if: 3 < 2] short1",
        "[option] ha [to: $whee()]",
        "[option] ba [to: $whee()]",
        "[end_group]",
        "[option_group] short2",
        "[option_if: 2 > 1] ha [to: $whee()]",
        "[option_if: 3 < 2] ba [to: $whee()]",
        "[end_group]");
  }

  @Test
  public void promptExpression() throws CompilerException {
    assertParses(
        "[define: foo PROMPT_ID.id1]",
        "[extern_script: $whee():BLOCK_ID]",
        "[prompt: id1] [no_text]",
        "[option] foo [to: $whee()]",
        "[option] bar [to: $whee()]");
    assertParses(
        "[extern_script: $foo(id:PROMPT_ID):VOID]",
        "[block: bid] text [do: $foo(PROMPT_ID.id1)]",
        "[prompt: id1] [no_text]",
        "[option] foo [to: bid]",
        "[option] bar [to: bid]");
    assertParses(
        "[define: foo PROMPT_ID.id1]",
        "[extern_script: $sfoo(id:PROMPT_ID):VOID]",
        "[block: bid] text [do: $sfoo(foo)]",
        "[prompt: id1] [no_text]",
        "[option] foo [to: bid]",
        "[option] bar [to: bid]");

    assertErrors("Prompt id 'doesnotexist'", "[define: foo PROMPT_ID.doesnotexist]");
    assertErrors(
        "Prompt id 'doesnotexist'",
        "[extern_script: $foo(id:PROMPT_ID):VOID]",
        "[block] text [do: $foo(PROMPT_ID.doesnotexist)]");
  }

  @Test
  public void ifElse() throws CompilerException {
    assertParses("[block] [if: 3 > 2] hi [else] whoa [end_if]");
    assertParses(
        "[extern_script: $bar():BOOLEAN]",
        "[block] [if: 3 > 2] hihi [else_if: $bar()] whoa [else_if: $bar()] wat [end_if]");
    assertParses(
        "[extern_script: $bar():BOOLEAN]",
        "[block: id] t [if: $bar()] [if: $bar()] [if: $bar()] [to: id] [else] [to: id] [end_if] [else] [to: id] [end_if] [else] [to: id] [end_if]");

    assertErrors("but was INTEGER", "[block] [if: 3] hi [else] whoa [end_if]");
    assertErrors("no [end_if] for this [if", "[block] [if: true] hi [else] whoa");
    assertErrors(
        "no [end_if] for this [if",
        "[extern_script: $bar():BOOLEAN]",
        "[block] [if: $bar()] whoa [else] [if: $bar()] what [else_if: $bar()] [end_if]");
    assertErrors(
        "[else] cannot follow [else]", "[block] [if: true] hi [else] whoa [else] what? [end_if]");
    assertErrors(
        "[else_if] cannot follow [else]",
        "[block] [if: true] hi [else] whoa [else_if: false] what? [end_if]");
    assertErrors("unexpected tag", "[block] [if: true] hi [end_if] [else_if: false] what [end_if]");
    assertErrors("unexpected tag", "[block] [else_if: true] whoa [end_if]");
    assertErrors("unexpected tag", "[block] [else] whoa [end_if]");
    assertErrors("unexpected tag", "[block] [end_if] whoa");
  }

  @Test
  public void define() throws CompilerException {
    assertParses("[define: foo 1]");
    assertParses("[define: foo 1.2]");
    assertParses("[define: foo \"literal\"]");
    assertParses("[define: foo True]");
    assertParses("[define: foo #AABBCC]");

    assertErrors("constant expected here", "[define: foo 1]", "[define: bar foo]");
    assertErrors("duplicate", "[define: foo 1]", "[define: foo 2]");
    assertErrors("attempting to assign DOUBLE", "[define: foo 1]", "[block] text [do: foo = 2.0]");
  }

  @Test
  public void defineKey() throws CompilerException {
    assertParses("[define: foo STRUCT.Key()]", "[block] [if: foo.obtained] whee [end_if]");
    assertParses("[define: foo STRUCT.Key(count=1)]", "[block] [if: foo.obtained] whee [end_if]");
    assertParses(
        "[define: foo STRUCT.Key(obtained=false)]", "[block] [if: foo.obtained] whee [end_if]");
    assertParses(
        "[define: foo STRUCT.Key(count=1, obtained=true)]",
        "[block] [if: foo.obtained] whee [end_if]");
    assertParses(
        "[define: foo STRUCT.Key(obtained=true, count=1)]",
        "[block] [if: foo.obtained] whee [end_if]");

    assertErrors(
        "constant expected",
        "[define: bar 3]",
        "[define: foo STRUCT.Key(count=bar)]",
        "[block] [if: foo.obtained] whee [end_if]");
  }

  @Test
  public void customStruct() throws CompilerException {
    assertParses(
        "[define_struct: thingy]",
        "[required_field: thing1 INTEGER]",
        "[field: thing2 0]",
        "[define: foo STRUCT.thingy(thing1=1)]",
        "[define: bar STRUCT.thingy(thing1=0, thing2=1)]",
        "[block] [do: foo.thing2 = bar.thing1] [no_text]");
    assertParses(
        "[extern_script: $makeKey2():STRUCT.key2]",
        "[define_struct: key2]",
        "[field: bb 1]",
        "[field: cc STRUCT.key1(aa=3)]",
        "[define_struct: key1]",
        "[field: aa 0]",
        "[define: foo1 STRUCT.key1()]",
        "[define: foo2 STRUCT.key1(aa=0)]",
        "[define: foo3 STRUCT.key1(aa=3)]",
        "[define: foo4 STRUCT.key2()]",
        "[define: foo5 STRUCT.key2(bb=2)]",
        "[define: foo6 STRUCT.key2(cc=STRUCT.key1())]",
        "[define: foo7 STRUCT.key2(cc=STRUCT.key1(aa=4))]",
        "[block] [do: foo4.cc.aa = foo7.cc.aa + $makeKey2().bb] [no_text]");
    assertParses(
        "[define_enum: enumtype_other val1 val2 val3]",
        "[define_struct: item]",
        "[required_field: name STRING]",
        "[field: count 0]",
        "[define_struct: inventory]",
        "[required_field: name STRING]",
        "[required_field: another_item STRUCT.item]",
        "[required_field: an_enum_var ENUM.enumtype_other]",
        "[field: spaces 0]",
        "[field: item STRUCT.item(name=\"thing\", count=0)]");

    assertErrors("struct has no defined fields", "[define_struct: thingy]", "[block] [no_text]");
    assertErrors(
        "is self-referential", "[define_struct: thingy]", "[required_field: thing1 STRUCT.thingy]");
    assertErrors(
        "is self-referential",
        "[define_struct: thingy1]",
        "[required_field: thing1 STRUCT.thingy2]",
        "[define_struct: thingy2]",
        "[required_field: thing2 STRUCT.thingy1]");
    assertErrors(
        "'bb' is undefined",
        "[define_struct: thing]",
        "[field: aa 0]",
        "[define: foo STRUCT.thing(bb=0)]");
    assertErrors(
        "duplicate kwarg: 'aa'",
        "[define_struct: thing]",
        "[field: aa 0]",
        "[define: foo STRUCT.thing(aa=1, aa=3)]");
    assertErrors(
        "missing required fields: 'aa'",
        "[define_struct: thing]",
        "[required_field: aa INTEGER]",
        "[define: foo STRUCT.thing()]");
  }

  @Test
  public void defineEnum() throws CompilerException {
    assertParses("[define_enum: Foo BAR BAZ]", "[define: var Foo.BAR]");
    assertParses(
        "[define_enum: Foo BAR BAZ]",
        "[define: var1 Foo.BAR]",
        "[define: var2 Foo.BAZ]",
        "[block] text [if: var1 == var2] then [else] [end_if]");

    assertErrors("at least two arguments", "[define_enum: NoArgs]");
    assertErrors("duplicate", "[define_enum: Enum1 Foo Bar]", "[define_enum: Enum1 Baz Qux]");
    assertErrors("duplicate", "[define_enum: Enum1 Foo Bar]", "[define: Enum1 1.2]");
    assertErrors("duplicate", "[define_enum: Things ONE TWO ONE THREE]");
    assertErrors(
        "enum type 'Weather' has no value 'CLOUDY'",
        "[define_enum: Weather SUNNY RAINY]",
        "[define: cur Weather.CLOUDY]");
    assertErrors(
        "operator 'ENUM.Weather == ENUM.Mood' is not defined",
        "[define_enum: Weather SUNNY RAINY]",
        "[define_enum: Mood HAPPY SAD]",
        "[define: var1 Weather.SUNNY]",
        "[define: var2 Mood.HAPPY]",
        "[block] text [if: var1 == var2] then [else] [end_if]");
  }

  @Test
  public void externScript() throws CompilerException {
    assertParses(
        "[extern_script: $func(a:INTEGER, b:INTEGER):BOOLEAN]",
        "[block: foo] [if: $func(2, 3)] haha [else] ohno [end_if]");
    assertParses(
        "[extern_script: $func(a:INTEGER, b:INTEGER):INTEGER]",
        "[block: foo] [if: $func($func(1, 4), 3) > $func(5, 7)] haha [else] ohno [end_if]");
    assertParses(
        "[define_enum: foo FooA FooB]",
        "[extern_script: $func(a:ENUM.foo):VOID]",
        "[extern_script: $func2():ENUM.foo]");

    assertErrors(
        "duplicate", "[extern_script: $foo():INTEGER]", "[extern_script: $foo(a:BOOLEAN):INTEGER]");
    assertErrors(
        "2 arguments, but found 3",
        "[extern_script: $foo(a:INTEGER, b:INTEGER):BOOLEAN]",
        "[block: heya] texty [if: $foo(1, 2, 3)] hi [end_if]");
    assertErrors(
        "2 arguments, but found 1",
        "[extern_script: $foo(a:INTEGER, b:INTEGER):BOOLEAN]",
        "[block: heya] texty [if: $foo(1)] hi [end_if]");
    assertErrors(
        "but was BOOLEAN",
        "[extern_script: $foo(a:INTEGER, b:INTEGER):BOOLEAN]",
        "[block: heya] texty [if: $foo(true, 7)] hi [end_if]");
  }

  @Test
  public void scriptTag() throws CompilerException {
    assertParses(
        "[extern_script: $foo(a:INTEGER):VOID]", "[script_tag: foo foo]", "[block] text [foo: 13]");
    assertParses(
        "[define_enum: Greek ALPHA BETA GAMMA]",
        "[extern_script: $foo(a:ENUM.Greek):VOID]",
        "[script_tag: foo foo]",
        "[block] text [foo: ALPHA]");
    assertParses(
        "[extern_script: $foo(a:INTEGER):VOID]",
        "[script_tag: foobar foo]",
        "[block] text [foobar: 7]");
    assertParses(
        "[extern_script: $foo(a:INTEGER):STRING]",
        "[script_tag: foobar foo]",
        "[block] text [foobar: 7]");
    assertParses(
        "[extern_script: $foo(a:INTEGER):STRING]",
        "[script_tag: foobar foo]",
        "[script_tag: fooqux foo]",
        "[block] text [foobar: 3] [fooqux: 5]");
    assertParses(
        "[extern_script: $foo(a:DOUBLE):STRING]",
        "[script_tag: foobar foo]",
        "[script_tag: fooqux foo]",
        "[block] text [foobar: 3.1] [fooqux: 5]");

    assertErrors("no extern_script named 'doesnotexist'", "[script_tag: foo doesnotexist]");
    assertErrors(
        "'block' is a reserved word",
        "[extern_script: $foo(a:INTEGER):VOID]",
        "[script_tag: block foo]");
    assertErrors(
        "expects 1 argument",
        "[extern_script: $foo(a:INTEGER):VOID]",
        "[script_tag: foo foo]",
        "[block] text [foo: 13 12]");
    assertErrors(
        "must return type STRING or VOID",
        "[extern_script: $foo(a:INTEGER):INTEGER]",
        "[script_tag: foo foo]");
    assertErrors(
        "duplicate script tag declaration",
        "[extern_script: $foo1(a:INTEGER):VOID]",
        "[extern_script: $foo2(a:INTEGER):VOID]",
        "[script_tag: foo foo2]",
        "[script_tag: foo foo1]");
    assertErrors(
        "expected type INTEGER here, but was BOOLEAN",
        "[extern_script: $foo(a:INTEGER):VOID]",
        "[script_tag: foo foo]",
        "[block] text [foo: true]");
    assertErrors(
        "but was 'DELTA'",
        "[define_enum: Greek ALPHA BETA GAMMA]",
        "[extern_script: $foo(a:ENUM.Greek):VOID]",
        "[script_tag: foo foo]",
        "[block] text [foo: DELTA]");
  }
}
