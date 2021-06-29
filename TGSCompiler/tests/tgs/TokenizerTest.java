package tgs;

import static com.google.common.truth.Truth.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

import com.google.common.collect.ImmutableList;
import com.google.common.truth.Correspondence;

public class TokenizerTest {

  private StringBuilder file = new StringBuilder();

  private void println(String line) {
    file.append(line);
    file.append('\n');
  }

  public ImmutableList<Tokenizer.TagOrText> tokenize() throws CompilerException {
    return new Tokenizer("/test/file.txt", file.toString()).tokenize();
  }

  @Test
  public void emptyFile() throws CompilerException {
    assertThat(tokenize()).isEmpty();
  }

  @Test
  public void singleLineComment() throws CompilerException {
    println("This is // comment one");
    println("text");

    ImmutableList<Tokenizer.TagOrText> tokenized = tokenize();

    assertThat(tokenized).hasSize(1);
    assertText(tokenized.get(0), "This is text");
  }

  @Test
  public void multiLineComment() throws CompilerException {
    println("This /*inline*/ is text /* that");
    println("spans multiple lines");
    println("spans */ multiple lines");

    ImmutableList<Tokenizer.TagOrText> tokenized = tokenize();

    assertThat(tokenized).hasSize(1);
    assertText(tokenized.get(0), "This is text multiple lines");
  }

  @Test
  public void basicTag() throws CompilerException {
    println("[tag]");

    ImmutableList<Tokenizer.TagOrText> tokenized = tokenize();

    assertThat(tokenized).hasSize(1);
    assertTag(tokenized.get(0), "tag");
  }

  @Test
  public void tagAndWhitespace() throws CompilerException {
    println("text[tag]text");
    println("moretext [tag]text");
    println("more more text[tag] text");
    println("moretext [tag] text");

    ImmutableList<Tokenizer.TagOrText> tokenized = tokenize();

    assertThat(tokenized).hasSize(13);
    assertText(tokenized.get(0), "text");
    assertTag(tokenized.get(1), "tag");
    assertText(tokenized.get(2), "text moretext");
    assertWhitespace(tokenized.get(3));
    assertTag(tokenized.get(4), "tag");
    assertText(tokenized.get(5), "text more more text");
    assertTag(tokenized.get(6), "tag");
    assertWhitespace(tokenized.get(7));
    assertText(tokenized.get(8), "text moretext");
    assertWhitespace(tokenized.get(9));
    assertTag(tokenized.get(10), "tag");
    assertWhitespace(tokenized.get(11));
    assertText(tokenized.get(12), "text");
  }

  @Test
  public void unexpectedOpenError() throws CompilerException {
    println("dododo ] what");

    assertThrows(CompilerException.class, this::tokenize);
  }

  @Test
  public void unclosedTagError() throws CompilerException {
    println("[tag: unclosed");

    assertThrows(CompilerException.class, this::tokenize);
  }

  @Test
  public void doubleOpenTagError() throws CompilerException {
    println("[tag: [inner]]");

    assertThrows(CompilerException.class, this::tokenize);
  }

  @Test
  public void commentInTag() throws CompilerException {
    println("[tag1 //comment");
    println("] [tag2: /*comment*/ arg]");

    ImmutableList<Tokenizer.TagOrText> tokenized = tokenize();

    assertThat(tokenized).hasSize(3);
    assertTag(tokenized.get(0), "tag1");
    assertWhitespace(tokenized.get(1));
    assertTag(tokenized.get(2), "tag2", "arg");
  }

  @Test
  public void quotes() throws CompilerException {
    println("[tag: with\" quoted text\" and non-quoted ]");

    ImmutableList<Tokenizer.TagOrText> tokenized = tokenize();

    assertThat(tokenized).hasSize(1);
    assertTag(tokenized.get(0), "tag", "with", "\" quoted text\"", "and", "non-quoted");
  }

  @Test
  public void unclosedQuoteError() throws CompilerException {
    println("[tag: \"quote]");

    assertThrows(CompilerException.class, this::tokenize);
  }

  @Test
  public void escapeQuotes() throws CompilerException {
    println("[tag: \"\\\"quote\\\"\" hi]");

    ImmutableList<Tokenizer.TagOrText> tokenized = tokenize();

    assertThat(tokenized).hasSize(1);
    assertTag(tokenized.get(0), "tag", "\"\\\"quote\\\"\"", "hi");
  }

  @Test
  public void invalidEscapeError() throws CompilerException {
    println("[tag: \"\\3\"]");

    assertThrows(CompilerException.class, this::tokenize);
  }

  private static void assertText(Tokenizer.TagOrText tagOrText, String text) {
    assertThat(tagOrText.isTag()).isFalse();
    assertThat(tagOrText.isWhitespace()).isFalse();
    assertThat(tagOrText.text().text()).isEqualTo(text);
  }

  private static void assertWhitespace(Tokenizer.TagOrText tagOrText) {
    assertThat(tagOrText.isTag()).isFalse();
    assertThat(tagOrText.isWhitespace()).isTrue();
  }

  private static void assertTag(Tokenizer.TagOrText tagOrText, String tagName, String... args) {
    assertThat(tagOrText.isTag()).isTrue();
    assertThat(tagOrText.isWhitespace()).isFalse();
    assertThat(tagOrText.tag().tagName()).isEqualTo(tagName);
    assertThat(tagOrText.tag().args())
        .comparingElementsUsing(tagArgs())
        .containsExactly(args)
        .inOrder();
  }

  private static Correspondence<Tokenizer.TagArg, String> tagArgs() {
    return Correspondence.from((a, s) -> a.arg().equals(s), "has string form equal to");
  }
}
