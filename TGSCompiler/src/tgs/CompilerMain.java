package tgs;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.google.common.collect.ImmutableList;
import com.google.common.io.Files;

public class CompilerMain {

  private static final String SYNTAX_FILE = "TextGameScript.sublime-syntax";

  private static final String AUTOCOMPLETE_FILE = "TextGameScript.autocomplete-db";

  public static void main(String[] args) throws CompilerException, IOException {
    if (args.length != 2) {
      System.err.println("Usage: $COMPILER tgs_file user_package_dir");
      System.exit(1);
    }

    // Tokenize.
    boolean success = true;
    Map<String, ImmutableList<Tokenizer.TagOrText>> tokens = new HashMap<>();
    for (File f : getFiles(args[0])) {
      Tokenizer t = new Tokenizer(f.toString(), read(f));
      try {
        tokens.put(f.getName(), t.tokenize());
      } catch (CompilerException ex) {
        ex.print();
        success = false;
        continue;
      }
    }

    // Generate the syntax file.
    File userDir = new File(args[1]);
    if (success) {
      SublimeSyntaxCompiler.Input.Builder syntaxInputBuilder =
          SublimeSyntaxCompiler.Input.builder();
      tokens.values().stream().flatMap(l -> l.stream()).forEach(syntaxInputBuilder::addTagOrText);

      File f = new File(userDir, SYNTAX_FILE);
      try {
        SublimeSyntaxCompiler syntaxCompiler =
            new SublimeSyntaxCompiler(syntaxInputBuilder.build());
        if (syntaxCompiler.needsChange(f)) {
          write(syntaxCompiler.compileRules(), new File(userDir, SYNTAX_FILE));
          System.out.println("Syntax file successfully updated!");
        } else {
          System.out.println("Syntax file is up-to-date");
        }
      } catch (CompilerException ex) {
        System.out.println("Syntax file not updated; there was a type error");
        ex.print();
      }
    } else {
      System.out.println("Syntax file not updated; There were tokenization errors.");
    }

    Map<String, AST> asts = new HashMap<>();
    if (success) {
      for (String f : tokens.keySet()) {
        try {
          AST.Builder astBuilder = new AST.Builder();
          for (Tokenizer.TagOrText tagOrText : tokens.get(f)) {
            if (tagOrText.isTag()) {
              astBuilder.consumeTag(TypedTag.parse(tagOrText.tag()));
            } else if (tagOrText.isWhitespace()) {
              astBuilder.consumeWhitespace(tagOrText.whitespace());
            } else {
              astBuilder.consumeText(tagOrText.text());
            }
          }
          asts.put(f, astBuilder.build());
        } catch (CompilerException ex) {
          ex.print();
          success = false;
          continue;
        }
      }
    }

    if (!success) {
      System.out.println("Compilation failed.  See errors above.");
      System.exit(1);
    }

    ASTValidator validator = new ASTValidator(asts);
    ImmutableList<CompilerException> errors = validator.computeErrors();

    if (validator.hasValidLabelRegistry()) {
      AutocompleteCompiler autocompleteCompiler =
          new AutocompleteCompiler(validator.labelRegistry());
      Files.asByteSink(new File(userDir, AUTOCOMPLETE_FILE)).write(autocompleteCompiler.compile());
    } else {
      System.out.println("Autocomplete file not updated; There were compilation errors.");
    }

    if (!errors.isEmpty()) {
      errors.stream().forEach(CompilerException::print);
      System.out.println("Compilation failed.  See errors above.");
      System.exit(1);
    }

    Compiler compiler =
        new Compiler(
            asts.entrySet()
                .stream()
                .sorted(Comparator.comparing(e -> e.getKey()))
                .map(e -> e.getValue())
                .collect(ImmutableList.toImmutableList()),
            validator.labelRegistry());
    compiler.compile();

    File dir = new File(args[0]).getParentFile();
    Files.asByteSink(new File(dir, "tgs.index")).write(compiler.indexFile());
    Files.asByteSink(new File(dir, "tgs.out")).write(compiler.outFile());

    System.out.println("Compilation succeeded!");
  }

  private static List<File> getFiles(String inFile) {
    return Arrays.asList(new File(inFile).getParentFile().listFiles())
        .stream()
        .filter(f -> f.isFile() && f.toString().endsWith(".tgs"))
        .collect(Collectors.toList());
  }

  private static String read(File file) throws IOException {
    return Files.asCharSource(file, StandardCharsets.UTF_8).read();
  }

  private static void write(String string, File file) throws IOException {
    Files.asCharSink(file, StandardCharsets.UTF_8).write(string);
  }
}
