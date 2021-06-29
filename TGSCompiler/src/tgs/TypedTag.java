package tgs;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

import com.google.common.base.Verify;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.io.ByteArrayDataOutput;

import tgs.Compiler.Registry;
import tgs.SublimeSyntaxCompiler.ArgsRule;
import tgs.SublimeSyntaxCompiler.TagRule;
import tgs.processor.ASTChild;
import tgs.processor.ASTNode;

public abstract class TypedTag implements ASTNodeInterface {
  @FunctionalInterface
  private interface Parser {
    TypedTag parse(Tokenizer.Tag tag) throws CompilerException;
  }

  @FunctionalInterface
  private interface TagRuleGenerator {
    TagRule tagRule();
  }

  public static enum Type {
    BLOCK(Block::new, Block::tagRule),
    BOLD_CLOSE(BoldClose::new, BoldClose::tagRule, "/b"),
    BOLD_OPEN(BoldOpen::new, BoldOpen::tagRule, "b"),
    CASE(Case::new, Case::tagRule),
    COLOR_CLOSE(ColorClose::new, ColorClose::tagRule, "/color"),
    COLOR_OPEN(ColorOpen::new, ColorOpen::tagRule, "color"),
    CUSTOM_TAG(CustomTag::new, null, true),
    DEFAULT(Default::new, Default::tagRule),
    DEFINE(Define::new, Define::tagRule),
    DEFINE_ENUM(DefineEnum::new, DefineEnum::tagRule),
    DEFINE_STRUCT(DefineStruct::new, DefineStruct::tagRule),
    DO(Do::new, Do::tagRule),
    ELSE(Else::new, Else::tagRule),
    ELSE_IF(ElseIf::new, ElseIf::tagRule),
    END_GROUP(EndGroup::new, EndGroup::tagRule),
    END_IF(EndIf::new, EndIf::tagRule),
    END_SWITCH(EndSwitch::new, EndSwitch::tagRule),
    EXTERN_BLOCK(ExternBlock::new, ExternBlock::tagRule),
    EXTERN_SCRIPT(ExternScript::new, ExternScript::tagRule),
    FIELD(Field::new, Field::tagRule),
    IF(If::new, If::tagRule),
    ITALICS_CLOSE(ItalicsClose::new, ItalicsClose::tagRule, "/i"),
    ITALICS_OPEN(ItalicsOpen::new, ItalicsOpen::tagRule, "i"),
    NO_TEXT(NoText::new, NoText::tagRule),
    OPTION(Option::new, Option::tagRule),
    OPTION_DEFAULT(OptionDefault::new, OptionDefault::tagRule),
    OPTION_GROUP(OptionGroup::new, OptionGroup::tagRule),
    OPTION_GROUP_IF(OptionGroupIf::new, OptionGroupIf::tagRule),
    OPTION_IF(OptionIf::new, OptionIf::tagRule),
    OPTION_NONE(OptionNone::new, OptionNone::tagRule),
    PROMPT(Prompt::new, Prompt::tagRule),
    REQUIRED_FIELD(RequiredField::new, RequiredField::tagRule),
    SCRIPT_TAG(ScriptTag::new, ScriptTag::tagRule),
    STRING(StringTag::new, StringTag::tagRule),
    SUB_BLOCK(SubBlock::new, SubBlock::tagRule),
    SWITCH(Switch::new, Switch::tagRule),
    TO(To::new, To::tagRule),
    URL_OPEN(UrlOpen::new, UrlOpen::tagRule, "url"),
    URL_CLOSE(UrlClose::new, UrlClose::tagRule, "/url"),
    USE(Use::new, Use::tagRule),
    USE_PROMPT(UsePrompt::new, UsePrompt::tagRule),
    USE_SUB_BLOCK(UseSubBlock::new, UseSubBlock::tagRule);

    private final Parser parser;
    private final TagRuleGenerator tagRuleGenerator;
    private TagRule tagRule = null;
    private final Optional<String> tagName;

    Type(Parser parser, TagRuleGenerator tagRuleGenerator) {
      this(parser, tagRuleGenerator, false);
    }

    Type(Parser parser, TagRuleGenerator tagRuleGenerator, boolean noTagName) {
      this.parser = parser;
      this.tagRuleGenerator = tagRuleGenerator;
      this.tagName = noTagName ? Optional.empty() : Optional.of(name().toLowerCase());
    }

    Type(Parser parser, TagRuleGenerator tagRuleGenerator, String tagName) {
      this.parser = parser;
      this.tagRuleGenerator = tagRuleGenerator;
      this.tagName = Optional.of(tagName);
    }

    public Optional<String> tagName() {
      return tagName;
    }

    private static final ImmutableList<TagRule> TAG_RULES;

    static {
      TAG_RULES =
          Arrays.asList(Type.values())
              .stream()
              .map(TypedTag.Type::tagRule)
              .filter(r -> r != null)
              .collect(ImmutableList.toImmutableList());
    }

    public static ImmutableList<TagRule> tagRules() {
      return TAG_RULES;
    }

    public TagRule tagRule() {
      if (tagRule != null) return tagRule;
      if (tagRuleGenerator == null) return null;

      tagRule = tagRuleGenerator.tagRule();
      Verify.verify(tagRule.tagName().equals(tagName().get()), "Bad TagRule for " + this);
      return tagRule;
    }

    public TypedTag parse(Tokenizer.Tag tag) throws CompilerException {
      return parser.parse(tag);
    }
  }

  private final Tokenizer.Tag tag;
  private final Type type;

  protected TypedTag(Type type, Tokenizer.Tag tag) {
    this.tag = tag;
    this.type = type;

    type.tagName().ifPresent(n -> Verify.verify(n.equals(tag.tagName())));
  }

  public final Type type() {
    return type;
  }

  public final String tagName() {
    return tag.tagName();
  }

  public final Tokenizer.Pos tagNamePos() {
    return tag.tagNamePos();
  }

  public final Tokenizer.Pos argPos(int index) {
    return tag.arg(index).pos();
  }

  public final ImmutableList<Tokenizer.TagArg> rawArgs() {
    return tag.args();
  }

  public boolean isBbCode() {
    return false;
  }

  @Override
  public String toString() {
    return tag.toString();
  }

  @SuppressWarnings("unchecked")
  public <T extends TypedTag> T cast() {
    return (T) this;
  }

  public <T extends TypedTag> T cast(Class<T> clazz) {
    return cast();
  }

  public abstract void compile(Compiler.Registry registry, ByteArrayDataOutput out)
      throws CompilerException;

  private static final ImmutableMap<String, Type> TYPE_MAP =
      Arrays.asList(Type.values())
          .stream()
          .filter(t -> t.tagName().isPresent())
          .collect(ImmutableMap.toImmutableMap(t -> t.tagName().get(), t -> t));

  public static TypedTag parse(Tokenizer.Tag tag) throws CompilerException {
    Type type = TYPE_MAP.get(tag.tagName());
    if (type == null) {
      return new CustomTag(tag);
    }

    return type.parse(tag);
  }

  @ASTNode
  public static class Block extends WithOptionalId<Expression.BlockId>
      implements TypedTag_Block_ASTNode {
    private Block(Tokenizer.Tag tag) throws CompilerException {
      super(Type.BLOCK, tag, Expression.BlockId::parse);
    }

    public static TagRule tagRule() {
      return TagRule.defaultWithArgs(Type.BLOCK, ArgsRule.ID.optional());
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class BoldClose extends TypedBBCodeCloseTag implements TypedTag_BoldClose_ASTNode {
    private BoldClose(Tokenizer.Tag tag) throws CompilerException {
      super(Type.BOLD_CLOSE, tag, BBCode.BOLD);
    }

    public static TagRule tagRule() {
      return TagRule.bbCodeNoArgs(Type.BOLD_CLOSE);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
      Compiler.ContentOpCodes.writeBBCodeLiteral(Compiler.ContentOpCodes.BBCode.BOLD_CLOSE, out);
    }
  }

  @ASTNode
  public static class BoldOpen extends TypedBBCodeOpenTagNoArgs
      implements TypedTag_BoldOpen_ASTNode {
    private BoldOpen(Tokenizer.Tag tag) throws CompilerException {
      super(Type.BOLD_OPEN, tag, BBCode.BOLD);
    }

    public static TagRule tagRule() {
      return TagRule.bbCodeNoArgs(Type.BOLD_OPEN);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
      Compiler.ContentOpCodes.writeBBCodeLiteral(Compiler.ContentOpCodes.BBCode.BOLD_OPEN, out);
    }
  }

  @ASTNode
  public static class Case extends TypedTag implements TypedTag_Case_ASTNode {
    private final ImmutableList<String> cases;

    private Case(Tokenizer.Tag tag) throws CompilerException {
      super(Type.CASE, tag);

      if (tag.numArgs() == 0) {
        throw new CompilerException(tag.tagNamePos(), "Expected 1 or more enum arguments");
      }

      ImmutableList.Builder<String> builder = ImmutableList.builder();
      for (Tokenizer.TagArg arg : tag.args()) {
        builder.add(Expression.validateId(arg));
      }
      cases = builder.build();
    }

    public ImmutableList<String> cases() {
      return cases;
    }

    public ImmutableList<Expression.EnumValueLiteral> enumCases(DefineEnum definition) {
      return rawArgs()
          .stream()
          .map(a -> new Expression.EnumValueLiteral(definition, a.arg(), a.pos()))
          .collect(ImmutableList.toImmutableList());
    }

    public static TagRule tagRule() {
      return TagRule.defaultWithArgs(
          Type.CASE, ArgsRule.ENUM_VALUE.then(ArgsRule.ENUM_VALUE.optional().repeating()));
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class ColorClose extends TypedBBCodeCloseTag
      implements TypedTag_ColorClose_ASTNode {
    private ColorClose(Tokenizer.Tag tag) throws CompilerException {
      super(Type.COLOR_CLOSE, tag, BBCode.COLOR);
    }

    public static TagRule tagRule() {
      return TagRule.bbCodeNoArgs(Type.COLOR_CLOSE);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
      Compiler.ContentOpCodes.writeBBCodeLiteral(Compiler.ContentOpCodes.BBCode.COLOR_CLOSE, out);
    }
  }

  @ASTNode
  public static class ColorOpen extends TypedBBCodeTag implements TypedTag_ColorOpen_ASTNode {
    private final Optional<Expression.HexColorLiteral> literal;
    private final Optional<Expression.Variable> variable;

    private ColorOpen(Tokenizer.Tag tag) throws CompilerException {
      super(Type.COLOR_OPEN, tag, BBCode.COLOR, false);

      if (tag.numArgs() == 0)
        throw new CompilerException(tag.tagNamePos(), "expected variable id or #hex");
      if (tag.numArgs() > 1) throw new CompilerException(tag.arg(1).pos(), "unexpected argument");

      Tokenizer.TagArg arg = tag.arg(0);
      if (arg.arg().startsWith("#")) {
        literal = Optional.of(Expression.HexColorLiteral.parse(arg.arg(), arg.pos()));
        variable = Optional.empty();
      } else {
        literal = Optional.empty();
        variable = Optional.of(Expression.Variable.parse(arg.arg(), arg.pos()));
      }
    }

    public boolean hasHexLiteral() {
      return literal.isPresent();
    }

    public Expression.HexColorLiteral hexLiteral() {
      return literal.get();
    }

    public Expression.Variable variable() {
      return variable.get();
    }

    public static TagRule tagRule() {
      return TagRule.bbCodeWithArgs(
          Type.COLOR_OPEN, ArgsRule.HEX_COLOR_LITERAL.or(ArgsRule.VARIABLE));
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
      Compiler.Writer value =
          hasHexLiteral() ? hexLiteral().writer(registry) : variable().writer(registry);
      Compiler.ContentOpCodes.writeColorOpen(value, out);
    }

    @Override
    public <V> V visitChildren(ASTVisitor<V> visitor, V value) {
      V value2 = literal.map(x -> x.accept(visitor, value)).orElse(value);
      return variable.map(x -> x.accept(visitor, value)).orElse(value2);
    }
  }

  @ASTNode
  public static class CustomTag extends TypedTag implements TypedTag_CustomTag_ASTNode {
    private ImmutableList<Expression> resolvedExpressions;

    private CustomTag(Tokenizer.Tag tag) {
      super(Type.CUSTOM_TAG, tag);
      resolvedExpressions = ImmutableList.of();
    }

    public ScriptTag scriptTag(LabelRegistry labelRegistry) {
      return labelRegistry.scriptTagDefinition(tagName()).get();
    }

    private Expression resolveCustomArg(
        LabelRegistry labelRegistry, Tokenizer.TagArg arg, Expression.TypeLiteral argType)
        throws CompilerException {
      Expression expr = Expression.parseExpression(ImmutableList.of(arg));
      return expr.constantValue(argType.literalType().valueType(), labelRegistry);
    }

    public void resolve(LabelRegistry labelRegistry) throws CompilerException {
      Optional<ScriptTag> scriptTag = labelRegistry.scriptTagDefinition(tagName());
      if (!scriptTag.isPresent()) {
        throw new CompilerException(
            tagNamePos(), String.format("tag '%s' is undefined", tagName()));
      }

      ExternScript externScript =
          labelRegistry.scriptDefinition(scriptTag.get().scriptName()).get();
      if (externScript.numArgs() != rawArgs().size()) {
        throw new CompilerException(
            tagNamePos(),
            String.format(
                "Custom tag '%s' expects %d arguments, but received %d",
                tagName(), externScript.numArgs(), rawArgs().size()));
      }

      ImmutableList.Builder<Expression> builder = ImmutableList.builder();
      for (int i = 0; i < externScript.numArgs(); i++) {
        builder.add(resolveCustomArg(labelRegistry, rawArgs().get(i), externScript.argType(i)));
      }
      resolvedExpressions = builder.build();
    }

    @ASTChild
    @Override
    public ImmutableList<Expression> resolvedExpressions() {
      return resolvedExpressions;
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
      Compiler.ExprOpCodes.writeScriptInvocation(
          registry.getScriptId(scriptTag(registry.labelRegistry()).scriptName()),
          resolvedExpressions
              .stream()
              .map(e -> e.writer(registry))
              .collect(ImmutableList.toImmutableList()),
          out);
    }
  }

  @ASTNode
  public static class Default extends WithNoArguments implements TypedTag_Default_ASTNode {
    private Default(Tokenizer.Tag tag) throws CompilerException {
      super(Type.DEFAULT, tag);
    }

    public static TagRule tagRule() {
      return TagRule.defaultNoArgs(Type.DEFAULT);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class Define extends TypedTag implements TypedTag_Define_ASTNode {
    private final Expression.Variable variable;
    private final Expression.Holder<Expression> value;

    private Define(Tokenizer.Tag tag) throws CompilerException {
      super(Type.DEFINE, tag);

      if (tag.numArgs() < 2) {
        throw new CompilerException(tag.tagNamePos(), "[define] requires at least two arguments");
      }

      this.variable = Expression.Variable.parse(tag.arg(0).arg(), tag.arg(0).pos());
      this.value =
          new Expression.Holder<Expression>(
              Expression.parseExpression(tag.args().subList(1, tag.numArgs())));
    }

    @ASTChild
    @Override
    public Expression.Variable variable() {
      return variable;
    }

    public Expression value() {
      return value.get();
    }

    @ASTChild
    @Override
    public Expression rawValue() {
      return value;
    }

    public static TagRule tagRule() {
      // TODO: Make CONSTANT_VALUE support struct initializers, and switch back to CONSTANT_VALUE
      return TagRule.defineTagWithArgs(
          Type.DEFINE, ArgsRule.VARIABLE.then(ArgsRule.ARBITRARY_EXPRESSION));
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class DefineEnum extends TypedTag implements TypedTag_DefineEnum_ASTNode {
    private final String typeName;
    private final Expression.ValueType.EnumValueType valueType;
    private final ImmutableSet<String> values;

    private DefineEnum(Tokenizer.Tag tag) throws CompilerException {
      super(Type.DEFINE_ENUM, tag);

      if (tag.numArgs() < 2) {
        throw new CompilerException(
            tag.tagNamePos(), "[define_enum] requires at least two arguments");
      }
      for (Tokenizer.TagArg arg : tag.args()) {
        Expression.validateId(arg.arg(), arg.pos());
      }

      this.typeName = tag.arg(0).arg();

      Set<String> values = new HashSet<>();
      for (int i = 1; i < tag.numArgs(); i++) {
        if (!values.add(tag.arg(i).arg()))
          throw new CompilerException(tag.arg(i).pos(), "duplicate enum value");
      }
      this.values = ImmutableSet.copyOf(values);
      this.valueType = Expression.ValueType.EnumValueType.of(this);
    }

    public String typeName() {
      return typeName;
    }

    public Expression.ValueType.EnumValueType valueType() {
      return valueType;
    }

    public boolean hasValue(String value) {
      return values.contains(value);
    }

    public ImmutableSet<String> values() {
      return values;
    }

    public static TagRule tagRule() {
      return TagRule.defineTagWithArgs(
          Type.DEFINE_ENUM,
          ArgsRule.ENUM_TYPE
              .then(ArgsRule.ENUM_VALUE)
              .then(ArgsRule.ENUM_VALUE.optional().repeating()));
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class DefineStruct extends TypedTag implements TypedTag_DefineStruct_ASTNode {
    private final String typeName;

    protected DefineStruct(Tokenizer.Tag tag) throws CompilerException {
      super(Type.DEFINE_STRUCT, tag);

      if (tag.numArgs() == 0) {
        throw new CompilerException(tag.tagNamePos(), "expected argument");
      } else if (tag.numArgs() > 1) {
        throw new CompilerException(tag.arg(1).pos(), "unexpected argument");
      }

      this.typeName = Expression.validateId(tag.arg(0));
    }

    public String typeName() {
      return typeName;
    }

    public static TagRule tagRule() {
      return TagRule.defineTagWithArgs(Type.DEFINE_STRUCT, ArgsRule.STRUCT_TYPE);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class Do extends WithExpression implements TypedTag_Do_ASTNode {
    private Do(Tokenizer.Tag tag) throws CompilerException {
      super(Type.DO, tag);
    }

    public static TagRule tagRule() {
      return TagRule.defaultWithArgs(Type.DO, ArgsRule.ARBITRARY_EXPRESSION);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
      Compiler.ContentOpCodes.writeExec(expr().writer(registry), out);
    }
  }

  @ASTNode
  public static class Else extends WithNoArguments implements TypedTag_Else_ASTNode {
    private Else(Tokenizer.Tag tag) throws CompilerException {
      super(Type.ELSE, tag);
    }

    public static TagRule tagRule() {
      return TagRule.defaultNoArgs(Type.ELSE);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class ElseIf extends WithExpression implements TypedTag_ElseIf_ASTNode {
    private ElseIf(Tokenizer.Tag tag) throws CompilerException {
      super(Type.ELSE_IF, tag);
    }

    public static TagRule tagRule() {
      return TagRule.defaultWithArgs(Type.ELSE_IF, ArgsRule.ARBITRARY_EXPRESSION);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class EndGroup extends WithNoArguments implements TypedTag_EndGroup_ASTNode {
    private EndGroup(Tokenizer.Tag tag) throws CompilerException {
      super(Type.END_GROUP, tag);
    }

    public static TagRule tagRule() {
      return TagRule.defaultNoArgs(Type.END_GROUP);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class EndIf extends WithNoArguments implements TypedTag_EndIf_ASTNode {
    private EndIf(Tokenizer.Tag tag) throws CompilerException {
      super(Type.END_IF, tag);
    }

    public static TagRule tagRule() {
      return TagRule.defaultNoArgs(Type.END_IF);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class EndSwitch extends WithNoArguments implements TypedTag_EndSwitch_ASTNode {
    private EndSwitch(Tokenizer.Tag tag) throws CompilerException {
      super(Type.END_SWITCH, tag);
    }

    public static TagRule tagRule() {
      return TagRule.defaultNoArgs(Type.END_SWITCH);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class ExternBlock extends TypedTag implements TypedTag_ExternBlock_ASTNode {
    private final Expression.BlockId blockId;

    private ExternBlock(Tokenizer.Tag tag) throws CompilerException {
      super(Type.EXTERN_BLOCK, tag);

      if (tag.numArgs() == 0) {
        throw new CompilerException(tag.tagNamePos(), "expected argument");
      }
      if (tag.numArgs() > 1) {
        throw new CompilerException(tag.arg(1).pos(), "unexpected argument");
      }

      this.blockId = Expression.BlockId.parse(tag.arg(0).arg(), tag.arg(0).pos());
    }

    @ASTChild
    @Override
    public Expression.BlockId id() {
      return blockId;
    }

    public static TagRule tagRule() {
      return TagRule.externTagWithArgs(Type.EXTERN_BLOCK, ArgsRule.ID);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static final class ExternScript extends TypedTag implements TypedTag_ExternScript_ASTNode {
    private final Expression.Holder<Expression> declaration; // Resolved later.

    private ExternScript(Tokenizer.Tag tag) throws CompilerException {
      super(Type.EXTERN_SCRIPT, tag);

      if (tag.numArgs() == 0) throw new CompilerException(tag.tagNamePos(), "expected argument");

      this.declaration = new Expression.Holder<>(Expression.parseExpression(tag.args()));
    }

    // Used for extracting the function name before the expression resolution phase.
    public Optional<String> rawFunctionName() {
      if (declaration.get().type() != Expression.Type.BINARY) return Optional.empty();

      Expression.Binary binary = declaration.get().cast();
      if (binary.lhs().type() != Expression.Type.SCRIPT) return Optional.empty();

      return Optional.of(binary.lhs().cast(Expression.Script.class).function());
    }

    @ASTChild
    @Override
    public Expression expressionHolder() {
      return declaration;
    }

    public Expression rawExpression() {
      return declaration.get();
    }

    private Expression.TypeAnnotatedExpression typeAnnotated() {
      return declaration.get().cast();
    }

    private Expression.Script script() {
      return typeAnnotated().expression().cast();
    }

    public String functionName() {
      return script().function();
    }

    public Expression.TypeLiteral returnType() {
      return declaration.get().cast(Expression.TypeAnnotatedExpression.class).annotation();
    }

    public int numArgs() {
      return script().numArgs();
    }

    public ImmutableList<Expression.TypeLiteral> argTypes() throws CompilerException {
      ImmutableList.Builder<Expression.TypeLiteral> builder = ImmutableList.builder();
      for (int i = 0; i < numArgs(); i++) {
        builder.add(argType(i));
      }
      return builder.build();
    }

    public Expression.TypeLiteral argType(int index) throws CompilerException {
      if (script().arg(index).type() != Expression.Type.TYPE_ANNOTATED_EXPRESSION) {
        throw new CompilerException(script().arg(index).pos(), "Expected type annotation here");
      }
      return script().arg(index).cast(Expression.TypeAnnotatedExpression.class).annotation();
    }

    public static TagRule tagRule() {
      return TagRule.externTagWithArgs(Type.EXTERN_SCRIPT, ArgsRule.SCRIPT_EXPRESSION);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class Field extends StructFieldTag implements TypedTag_Field_ASTNode {
    protected Field(Tokenizer.Tag tag) throws CompilerException {
      super(Type.FIELD, tag);
    }

    @Override
    public StructType.Field getTypedField(LabelRegistry labelRegistry) throws CompilerException {
      return StructType.Field.create(rawArgs().get(0), value().resolve(labelRegistry));
    }

    public static TagRule tagRule() {
      return TagRule.defineTagWithArgs(
          Type.FIELD, ArgsRule.VARIABLE.then(ArgsRule.ARBITRARY_EXPRESSION));
    }
  }

  @ASTNode
  public static class If extends WithExpression implements TypedTag_If_ASTNode {
    private If(Tokenizer.Tag tag) throws CompilerException {
      super(Type.IF, tag);
    }

    public static TagRule tagRule() {
      return TagRule.defaultWithArgs(Type.IF, ArgsRule.ARBITRARY_EXPRESSION);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class ItalicsClose extends TypedBBCodeCloseTag
      implements TypedTag_ItalicsClose_ASTNode {
    private ItalicsClose(Tokenizer.Tag tag) throws CompilerException {
      super(Type.ITALICS_CLOSE, tag, BBCode.ITALICS);
    }

    public static TagRule tagRule() {
      return TagRule.bbCodeNoArgs(Type.ITALICS_CLOSE);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
      Compiler.ContentOpCodes.writeBBCodeLiteral(Compiler.ContentOpCodes.BBCode.ITALICS_CLOSE, out);
    }
  }

  @ASTNode
  public static class ItalicsOpen extends TypedBBCodeOpenTagNoArgs
      implements TypedTag_ItalicsOpen_ASTNode {
    private ItalicsOpen(Tokenizer.Tag tag) throws CompilerException {
      super(Type.ITALICS_OPEN, tag, BBCode.ITALICS);
    }

    public static TagRule tagRule() {
      return TagRule.bbCodeNoArgs(Type.ITALICS_OPEN);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
      Compiler.ContentOpCodes.writeBBCodeLiteral(Compiler.ContentOpCodes.BBCode.ITALICS_OPEN, out);
    }
  }

  @ASTNode
  public static class NoText extends WithNoArguments implements TypedTag_NoText_ASTNode {
    private NoText(Tokenizer.Tag tag) throws CompilerException {
      super(Type.NO_TEXT, tag);
    }

    public static TagRule tagRule() {
      return TagRule.defaultNoArgs(Type.NO_TEXT);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) {
      // No-op, just a sanity check.
    }
  }

  @ASTNode
  public static class Option extends WithNoArguments implements TypedTag_Option_ASTNode {
    private Option(Tokenizer.Tag tag) throws CompilerException {
      super(Type.OPTION, tag);
    }

    public static TagRule tagRule() {
      return TagRule.defaultNoArgs(Type.OPTION);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class OptionDefault extends WithNoArguments
      implements TypedTag_OptionDefault_ASTNode {
    private OptionDefault(Tokenizer.Tag tag) throws CompilerException {
      super(Type.OPTION_DEFAULT, tag);
    }

    public static TagRule tagRule() {
      return TagRule.defaultNoArgs(Type.OPTION_DEFAULT);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class OptionGroup extends WithNoArguments implements TypedTag_OptionGroup_ASTNode {
    private OptionGroup(Tokenizer.Tag tag) throws CompilerException {
      super(Type.OPTION_GROUP, tag);
    }

    public static TagRule tagRule() {
      return TagRule.defaultNoArgs(Type.OPTION_GROUP);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class OptionGroupIf extends WithExpression
      implements TypedTag_OptionGroupIf_ASTNode {
    private OptionGroupIf(Tokenizer.Tag tag) throws CompilerException {
      super(Type.OPTION_GROUP_IF, tag);
    }

    public static TagRule tagRule() {
      return TagRule.defaultWithArgs(Type.OPTION_GROUP_IF, ArgsRule.ARBITRARY_EXPRESSION);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class OptionIf extends WithExpression implements TypedTag_OptionIf_ASTNode {
    private OptionIf(Tokenizer.Tag tag) throws CompilerException {
      super(Type.OPTION_IF, tag);
    }

    public static TagRule tagRule() {
      return TagRule.defaultWithArgs(Type.OPTION_IF, ArgsRule.ARBITRARY_EXPRESSION);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class OptionNone extends WithNoArguments implements TypedTag_OptionNone_ASTNode {
    private OptionNone(Tokenizer.Tag tag) throws CompilerException {
      super(Type.OPTION_NONE, tag);
    }

    public static TagRule tagRule() {
      return TagRule.defaultNoArgs(Type.OPTION_NONE);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class Prompt extends WithOptionalId<Expression.PromptId>
      implements TypedTag_Prompt_ASTNode {
    private Prompt(Tokenizer.Tag tag) throws CompilerException {
      super(Type.PROMPT, tag, Expression.PromptId::parse);
    }

    public static TagRule tagRule() {
      return TagRule.defaultWithArgs(Type.PROMPT, ArgsRule.ID.optional());
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class RequiredField extends StructFieldTag
      implements TypedTag_RequiredField_ASTNode {
    protected RequiredField(Tokenizer.Tag tag) throws CompilerException {
      super(Type.REQUIRED_FIELD, tag);
    }

    @Override
    public StructType.Field getTypedField(LabelRegistry labelRegistry) throws CompilerException {
      Expression typeLiteral = value().resolve(labelRegistry);
      if (typeLiteral.type() != Expression.Type.TYPE_LITERAL) {
        throw new CompilerException(typeLiteral.pos(), "expected type literal here");
      }
      Expression.TypeLiteral typed = typeLiteral.cast();

      return StructType.Field.create(rawArgs().get(0), typed.literalType().valueType());
    }

    public static TagRule tagRule() {
      return TagRule.defineTagWithArgs(
          Type.REQUIRED_FIELD, ArgsRule.VARIABLE.then(ArgsRule.ARBITRARY_EXPRESSION));
    }
  }

  @ASTNode
  public static class ScriptTag extends TypedTag implements TypedTag_ScriptTag_ASTNode {
    private final String definedTagName;
    private final String scriptName;

    private ScriptTag(Tokenizer.Tag tag) throws CompilerException {
      super(Type.SCRIPT_TAG, tag);

      if (tag.numArgs() < 2)
        throw new CompilerException(tag.tagNamePos(), "Expected additional arguments");
      if (tag.numArgs() > 2) throw new CompilerException(tag.arg(2).pos(), "Unexpected argument");

      definedTagName = Expression.validateId(tag.arg(0));
      scriptName = Expression.validateId(tag.arg(1));
    }

    public String definedTagName() {
      return definedTagName;
    }

    public String scriptName() {
      return scriptName;
    }

    public static TagRule tagRule() {
      return TagRule.externTagWithArgs(
          Type.SCRIPT_TAG, ArgsRule.EXTERN_ID_LIGHT.then(ArgsRule.EXTERN_ID_DARK));
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class StringTag extends WithExpression implements TypedTag_StringTag_ASTNode {
    private StringTag(Tokenizer.Tag tag) throws CompilerException {
      super(Type.STRING, tag);
    }

    public static TagRule tagRule() {
      return TagRule.defaultWithArgs(Type.STRING, ArgsRule.ARBITRARY_EXPRESSION);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
      Compiler.ContentOpCodes.writeText(expr().writer(registry), out);
    }
  }

  @ASTNode
  public static class SubBlock extends TypedTag implements TypedTag_SubBlock_ASTNode {
    private final Expression.SubBlockId id;

    private SubBlock(Tokenizer.Tag tag) throws CompilerException {
      super(Type.SUB_BLOCK, tag);

      if (tag.numArgs() == 0) {
        throw new CompilerException(tag.tagNamePos(), "Expected argument");
      } else if (tag.numArgs() >= 2) {
        throw new CompilerException(tag.arg(1).pos(), "Unexpected argument");
      }

      id = Expression.SubBlockId.parse(tag.arg(0).arg(), tag.arg(0).pos());
    }

    @ASTChild
    @Override
    public Expression.SubBlockId id() {
      return id;
    }

    public static TagRule tagRule() {
      return TagRule.defaultWithArgs(Type.SUB_BLOCK, ArgsRule.ID);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class Switch extends WithExpression implements TypedTag_Switch_ASTNode {
    private Switch(Tokenizer.Tag tag) throws CompilerException {
      super(Type.SWITCH, tag);
    }

    public static TagRule tagRule() {
      return TagRule.defaultWithArgs(Type.SWITCH, ArgsRule.ARBITRARY_EXPRESSION);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class To extends WithBlockIdOrScript implements TypedTag_To_ASTNode {
    private To(Tokenizer.Tag tag) throws CompilerException {
      super(Type.TO, tag);
    }

    public static TagRule tagRule() {
      return TagRule.defaultWithArgs(Type.TO, ArgsRule.ID_OR_SCRIPT_EXPRESSION);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
      Compiler.InternalOps.writeVariableAssignment(
          registry, Compiler.InternalVars.NEXT_BLOCK_ID, store.writer(registry), out);
    }
  }

  @ASTNode
  public static class UrlClose extends TypedBBCodeCloseTag implements TypedTag_UrlClose_ASTNode {
    private UrlClose(Tokenizer.Tag tag) throws CompilerException {
      super(Type.URL_CLOSE, tag, BBCode.URL);
    }

    public static TagRule tagRule() {
      return TagRule.bbCodeNoArgs(Type.URL_CLOSE);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
      Compiler.ContentOpCodes.writeBBCodeLiteral(Compiler.ContentOpCodes.BBCode.URL_CLOSE, out);
    }
  }

  @ASTNode
  public static class UrlOpen extends TypedBBCodeTag implements TypedTag_UrlOpen_ASTNode {
    private final WithIdOrScript.Store<Expression.BlockId> store;

    private UrlOpen(Tokenizer.Tag tag) throws CompilerException {
      super(Type.URL_OPEN, tag, BBCode.URL, false);

      this.store =
          WithIdOrScript.Store.parse(tag.args(), tag.tagNamePos(), Expression.BlockId::parse);
    }

    public boolean hasScript() {
      return store.script.isPresent();
    }

    public Expression.Script script() {
      return store.script.get().get();
    }

    public Expression.BlockId blockId() {
      return store.id.get();
    }

    @ASTChild
    @Override
    public ASTNodeInterface idOrScript() {
      return store.idOrScript();
    }

    public static TagRule tagRule() {
      return TagRule.bbCodeWithArgs(Type.URL_OPEN, ArgsRule.ID_OR_SCRIPT_EXPRESSION);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
      Compiler.ContentOpCodes.writeUrlOpen(store.writer(registry), out);
    }
  }

  @ASTNode
  public static class Use extends TypedTag implements TypedTag_Use_ASTNode {
    private final Expression.Variable key;
    private final WithIdOrScript.Store<Expression.BlockId> store;

    private Use(Tokenizer.Tag tag) throws CompilerException {
      super(Type.USE, tag);

      if (tag.numArgs() < 2)
        throw new CompilerException(tag.tagNamePos(), "expected exactly 2 arguments");

      this.key = Expression.Variable.parse(tag.arg(0).arg(), tag.arg(0).pos());
      this.store =
          WithIdOrScript.Store.parse(
              tag.args().subList(1, tag.args().size()),
              tag.tagNamePos(),
              Expression.BlockId::parse);
    }

    @ASTChild
    @Override
    public Expression.Variable key() {
      return key;
    }

    public boolean hasScript() {
      return store.script.isPresent();
    }

    public Expression.Script script() {
      return store.script.get().get();
    }

    public Expression.BlockId blockId() {
      return store.id.get();
    }

    @ASTChild
    @Override
    public ASTNodeInterface idOrScript() {
      return store.idOrScript();
    }

    public static TagRule tagRule() {
      return TagRule.defaultWithArgs(
          Type.USE, ArgsRule.VARIABLE.then(ArgsRule.ID_OR_SCRIPT_EXPRESSION));
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
      Compiler.Writer key =
          o -> Compiler.ExprOpCodes.writeVariableReference(registry.getVariableId(key().name()), o);
      Compiler.Writer target = store.writer(registry);
      Compiler.InternalOps.writeFunctionCall(
          registry, Compiler.InternalScripts.SET_KEY_TARGET, ImmutableList.of(key, target), out);
    }
  }

  @ASTNode
  public static class UsePrompt extends WithPromptIdOrScript implements TypedTag_UsePrompt_ASTNode {
    private UsePrompt(Tokenizer.Tag tag) throws CompilerException {
      super(Type.USE_PROMPT, tag);
    }

    public static TagRule tagRule() {
      return TagRule.defaultWithArgs(Type.USE_PROMPT, ArgsRule.ID_OR_SCRIPT_EXPRESSION);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
      Compiler.InternalOps.writeVariableAssignment(
          registry, Compiler.InternalVars.CURRENT_PROMPT_ID, store.writer(registry), out);
    }
  }

  @ASTNode
  public static class UseSubBlock extends TypedTag implements TypedTag_UseSubBlock_ASTNode {
    private final Expression.SubBlockId id;

    private UseSubBlock(Tokenizer.Tag tag) throws CompilerException {
      super(Type.USE_SUB_BLOCK, tag);

      if (tag.numArgs() == 0) {
        throw new CompilerException(tag.tagNamePos(), "Expected id");
      } else if (tag.numArgs() >= 2) {
        throw new CompilerException(tag.arg(1).pos(), "Unexpected argument");
      }

      id = Expression.SubBlockId.parse(tag.arg(0).arg(), tag.arg(0).pos());
    }

    @ASTChild
    @Override
    public Expression.SubBlockId id() {
      return id;
    }

    public static TagRule tagRule() {
      return TagRule.defaultWithArgs(Type.USE_SUB_BLOCK, ArgsRule.ID);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
      Compiler.ContentOpCodes.writeExecSubBlock(registry.getSubBlockId(id), out);
    }

    @Override
    public <V> V accept(ASTVisitor<V> visitor, V value) {
      return visitor.visit(this, value);
    }
  }

  private abstract static class WithNoArguments extends TypedTag {
    protected WithNoArguments(Type type, Tokenizer.Tag tag) throws CompilerException {
      super(type, tag);

      if (tag.numArgs() > 0) {
        throw new CompilerException(
            tag.arg(0).pos(), String.format("unexpected argument for tag [%s]", tag.tagName()));
      }
    }

    @Override
    public final <V> V visitChildren(ASTVisitor<V> visitor, V value) {
      return value;
    }
  }

  public abstract static class WithExpression extends TypedTag {
    private final Expression.Holder<Expression> expr;

    protected WithExpression(Type type, Tokenizer.Tag tag) throws CompilerException {
      super(type, tag);

      if (tag.numArgs() == 0)
        throw new CompilerException(tag.tagNamePos(), "expected expression argument");
      this.expr = new Expression.Holder<>(Expression.parseExpression(tag.args()));
    }

    public Expression expr() {
      return expr.get();
    }

    @Override
    public final <V> V visitChildren(ASTVisitor<V> visitor, V value) {
      return expr.accept(visitor, value);
    }
  }

  public abstract static class WithIdOrScript<IdT extends Expression> extends TypedTag {
    @FunctionalInterface
    private interface Parser<IdT extends Expression> {
      IdT parse(String arg, Tokenizer.Pos pos) throws CompilerException;
    }

    private static class Store<IdT extends Expression> {
      private final Optional<IdT> id;
      private final Optional<Expression.Holder<Expression.Script>> script;

      private Store(Optional<IdT> id, Optional<Expression.Holder<Expression.Script>> script) {
        this.id = id;
        this.script = script;
      }

      public ASTNodeInterface idOrScript() {
        return id.isPresent() ? id.get() : script.get();
      }

      public Compiler.Writer writer(Compiler.Registry registry) {
        return id.isPresent() ? id.get().writer(registry) : script.get().writer(registry);
      }

      private static <IdT extends Expression> Store<IdT> parse(
          ImmutableList<Tokenizer.TagArg> args, Tokenizer.Pos startPos, Parser<IdT> parser)
          throws CompilerException {
        if (args.isEmpty()) throw new CompilerException(startPos, "expected argument");

        IdT id = null;
        if (args.size() == 1) {
          try {
            id = parser.parse(args.get(0).arg(), args.get(0).pos());
          } catch (CompilerException ex) {
          }
        }

        Expression.Holder<Expression.Script> script = null;
        if (id == null) {
          Expression expr = Expression.parseExpression(args);
          if (expr.type() != Expression.Type.SCRIPT)
            throw new CompilerException(expr.pos(), "expected id or $script");
          script = new Expression.Holder<>(expr.cast());
        }

        return new Store<>(Optional.ofNullable(id), Optional.ofNullable(script));
      }

      private final <V> V visitChildren(ASTVisitor<V> visitor, V value) {
        V value2 = id.map(x -> x.accept(visitor, value)).orElse(value);
        return script.map(x -> x.accept(visitor, value2)).orElse(value2);
      }
    }

    protected final Store<IdT> store;

    protected WithIdOrScript(Type type, Tokenizer.Tag tag, Parser<IdT> parser)
        throws CompilerException {
      super(type, tag);

      this.store = Store.parse(tag.args(), tag.tagNamePos(), parser);
    }

    public final boolean isScript() {
      return store.script.isPresent();
    }

    public final Expression.Script script() {
      return store.script.get().get();
    }

    @Override
    public final <V> V visitChildren(ASTVisitor<V> visitor, V value) {
      return store.visitChildren(visitor, value);
    }
  }

  public abstract static class WithBlockIdOrScript extends WithIdOrScript<Expression.BlockId> {
    protected WithBlockIdOrScript(Type type, Tokenizer.Tag tag) throws CompilerException {
      super(type, tag, Expression.BlockId::parse);
    }

    public final Expression.BlockId blockId() {
      return store.id.get();
    }
  }

  public abstract static class WithPromptIdOrScript extends WithIdOrScript<Expression.PromptId> {
    protected WithPromptIdOrScript(Type type, Tokenizer.Tag tag) throws CompilerException {
      super(type, tag, Expression.PromptId::parse);
    }

    public final Expression.PromptId promptId() {
      return store.id.get();
    }
  }

  public abstract static class WithOptionalId<IdT extends Expression> extends TypedTag {
    @FunctionalInterface
    private interface Parser<IdT extends Expression> {
      IdT parse(String arg, Tokenizer.Pos pos) throws CompilerException;
    }

    private final Optional<IdT> id;

    protected WithOptionalId(Type type, Tokenizer.Tag tag, Parser<IdT> parser)
        throws CompilerException {
      super(type, tag);

      if (tag.numArgs() == 0) {
        id = Optional.empty();
      } else if (tag.numArgs() > 1) {
        throw new CompilerException(tag.arg(1).pos(), "unexpected argument");
      } else {
        id = Optional.of(parser.parse(tag.arg(0).arg(), tag.arg(0).pos()));
      }
    }

    public final boolean hasId() {
      return id.isPresent();
    }

    public final IdT id() {
      return id.get();
    }

    @Override
    public final <V> V visitChildren(ASTVisitor<V> visitor, V value) {
      return id.map(x -> x.accept(visitor, value)).orElse(value);
    }
  }

  public abstract static class TypedBBCodeTag extends TypedTag {
    private final BBCode bbCode;
    private final boolean isClose;

    protected TypedBBCodeTag(Type type, Tokenizer.Tag tag, BBCode bbCode, boolean isClose) {
      super(type, tag);

      this.bbCode = bbCode;
      this.isClose = isClose;
    }

    @Override
    public final boolean isBbCode() {
      return true;
    }

    public final BBCode bbCode() {
      return bbCode;
    }

    public final boolean isClose() {
      return isClose;
    }
  }

  public abstract static class TypedBBCodeOpenTagNoArgs extends TypedBBCodeTag {
    protected TypedBBCodeOpenTagNoArgs(Type type, Tokenizer.Tag tag, BBCode bbCode)
        throws CompilerException {
      super(type, tag, bbCode, false);

      if (tag.numArgs() > 0) throw new CompilerException(tag.arg(0).pos(), "unexpected argument");
    }

    @Override
    public final <V> V visitChildren(ASTVisitor<V> visitor, V value) {
      return value;
    }
  }

  public abstract static class TypedBBCodeCloseTag extends TypedBBCodeTag {
    protected TypedBBCodeCloseTag(Type type, Tokenizer.Tag tag, BBCode bbCode)
        throws CompilerException {
      super(type, tag, bbCode, true);

      if (tag.numArgs() > 0) throw new CompilerException(tag.arg(0).pos(), "unexpected argument");
    }

    @Override
    public final <V> V visitChildren(ASTVisitor<V> visitor, V value) {
      return value;
    }
  }

  public abstract static class StructFieldTag extends TypedTag implements ASTNodeInterface {
    private final String fieldName;
    private final Expression.Holder<Expression> value;

    protected StructFieldTag(Type type, Tokenizer.Tag tag) throws CompilerException {
      super(type, tag);

      if (tag.numArgs() < 2) {
        throw new CompilerException(tag.tagNamePos(), "expected argument");
      }

      fieldName = Expression.validateId(tag.arg(0));
      value =
          new Expression.Holder<>(Expression.parseExpression(tag.args().subList(1, tag.numArgs())));
    }

    public abstract StructType.Field getTypedField(LabelRegistry labelRegistry)
        throws CompilerException;

    public String fieldName() {
      return fieldName;
    }

    public Tokenizer.Pos fieldNamePos() {
      return rawArgs().get(0).pos();
    }

    public Expression value() {
      return value.get();
    }

    @Override
    public final <V> V visitChildren(ASTVisitor<V> visitor, V value) {
      return this.value.accept(visitor, value);
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
      throw new UnsupportedOperationException();
    }
  }
}
