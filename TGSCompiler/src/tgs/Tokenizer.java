package tgs;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import com.google.auto.value.AutoValue;
import com.google.common.base.Splitter;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;

import tgs.processor.ASTNode;

/** Produces a tokenization of the input. */
public class Tokenizer {
  public static class Pos implements Comparable<Pos> {
    private static final Pos INTERNAL = new Pos("<internal>", -1, -1);

    public static Pos internal() {
      return INTERNAL;
    }

    private final String file;
    private final int lineNumber;
    private final int column;

    public Pos(String file, int lineNumber, int column) {
      this.file = file;
      this.lineNumber = lineNumber;
      this.column = column;
    }

    public String file() {
      return file;
    }

    public int lineNumber() {
      return lineNumber;
    }

    public int column() {
      return column;
    }

    public Pos addColumns(int columns) {
      return new Pos(file, lineNumber, column + columns);
    }

    @Override
    public int compareTo(Pos pos) {
      return Comparator.<Pos, String>comparing(p -> p.file().toString())
          .thenComparing(Pos::lineNumber)
          .thenComparing(Pos::column)
          .compare(this, pos);
    }
  }

  public static class TagArg {
    private final String arg;
    private final Pos pos;

    public TagArg(String arg, Pos pos) {
      this.arg = arg;
      this.pos = pos;
    }

    public String arg() {
      return arg;
    }

    public Pos pos() {
      return pos;
    }

    @Override
    public String toString() {
      return arg;
    }
  }

  // [name: arg0, arg1] text
  @AutoValue
  public abstract static class Tag {
    public abstract String tagName();

    public abstract Pos tagNamePos();

    public abstract ImmutableList<TagArg> args();

    public int numArgs() {
      return args().size();
    }

    public TagArg arg(int index) {
      return args().get(index);
    }

    public static Tag create(String tagName, Pos tagNamePos, Iterable<? extends TagArg> args) {
      return new AutoValue_Tokenizer_Tag(tagName, tagNamePos, ImmutableList.copyOf(args));
    }

    @Override
    public String toString() {
      if (args().isEmpty()) {
        return "[" + tagName() + "]";
      } else {
        return args()
            .stream()
            .map(TagArg::toString)
            .collect(Collectors.joining(" ", "[" + tagName() + ": ", "]"));
      }
    }
  }

  @ASTNode
  @AutoValue
  public abstract static class Text implements Tokenizer_Text_ASTNode {
    public abstract String text();

    public abstract Tokenizer.Pos startPos();

    public static Text create(String text, Tokenizer.Pos startPos) {
      return new AutoValue_Tokenizer_Text(text, startPos);
    }
  }

  @ASTNode
  @AutoValue
  public abstract static class Whitespace implements Tokenizer_Whitespace_ASTNode {
    public abstract Tokenizer.Pos startPos();

    public static Whitespace create(Tokenizer.Pos startPos) {
      return new AutoValue_Tokenizer_Whitespace(startPos);
    }
  }

  public static class TagOrText {
    private final Optional<Tag> tag;
    private final Optional<Text> text;
    private final Optional<Whitespace> whitespace;

    private TagOrText(Optional<Tag> tag, Optional<Text> text, Optional<Whitespace> whitespace) {
      this.tag = tag;
      this.text = text;
      this.whitespace = whitespace;
    }

    public static TagOrText tag(Tag tag) {
      return new TagOrText(Optional.of(tag), Optional.empty(), Optional.empty());
    }

    public static TagOrText text(Text text) {
      return new TagOrText(Optional.empty(), Optional.of(text), Optional.empty());
    }

    public static TagOrText whitespace(Whitespace whitespace) {
      return new TagOrText(Optional.empty(), Optional.empty(), Optional.of(whitespace));
    }

    public boolean isTag() {
      return tag.isPresent();
    }

    public boolean isWhitespace() {
      return whitespace.isPresent();
    }

    public Tag tag() {
      return tag.get();
    }

    public Text text() {
      return text.get();
    }

    public Whitespace whitespace() {
      return whitespace.get();
    }

    @Override
    public String toString() {
      if (isTag()) {
        return "[tag: " + tag() + "]";
      } else if (isWhitespace()) {
        return "[whitespace: " + whitespace() + "]";
      } else {
        return "[text: " + text() + "]";
      }
    }
  }

  private enum State {
    TEXT,
    TEXT_WORD,
    TAG_NAME_START,
    TAG_NAME_WORD,
    TAG_CLOSE_NO_ARGS,
    TAG_ARG_START,
    TAG_ARG_WORD,
    TAG_ARG_QUOTE;
  }

  private final String file;
  private final ImmutableList<String> lines;
  private int line = 0;
  private int col = -1; // In the initial state we have not read anything yet.
  private char ch = ' ';
  private State state = State.TEXT;

  private String tagName = null;
  private Pos tagNamePos = null;

  // Used both for tag arguments and narrative text.
  private StringBuilder textWord = new StringBuilder();
  private Pos textPos = null;
  private List<String> textWords = new ArrayList<>();
  private Optional<Whitespace> trailingWhitespace = Optional.empty();
  private Pos tagArgPos = null;
  private List<TagArg> tagArgs = new ArrayList<>();

  private final ImmutableList.Builder<TagOrText> tagOrTextsBuilder = ImmutableList.builder();

  public Tokenizer(String file, String content) {
    this.file = file;
    this.lines =
        ImmutableList.copyOf(Iterables.transform(Splitter.on('\n').split(content), s -> s + "\n"));
  }

  public static final char QUOTE = '\"';
  private static final char ILLEGAL_QUOTE = '\'';

  public ImmutableList<TagOrText> tokenize() throws CompilerException {
    while (advance()) {
      switch (state) {
        case TEXT:
          {
            if (!trailingWhitespace.isPresent()) {
              trailingWhitespace = readCommentsOrWhitespaceLoop();
              if (trailingWhitespace.isPresent()) {
                if (textWords.isEmpty()) {
                  tagOrTextsBuilder.add(TagOrText.whitespace(trailingWhitespace.get()));
                  trailingWhitespace = Optional.empty();
                }
                break;
              }
            }

            if (ch == '[') {
              closeTextStartTag();
              break;
            } else if (ch == ']') {
              throw error("unexpected ']'");
            }

            trailingWhitespace = Optional.empty();
            if (!readEscapeChar(false)) {
              textWord.append(ch);
            }

            if (textWords.isEmpty()) {
              textPos = pos();
            }
            state = State.TEXT_WORD;
            break;
          }
        case TEXT_WORD:
          {
            trailingWhitespace = readCommentsOrWhitespaceLoop();
            if (trailingWhitespace.isPresent()) {
              textWords.add(textWord.toString());
              textWord = new StringBuilder();
              state = State.TEXT;
              break;
            } else if (readEscapeChar(false)) {
              break;
            } else if (ch == '[') {
              closeTextStartTag();
              break;
            } else if (ch == ']') {
              throw error("unexpected ']'");
            }

            // Otherwise, grow the word.
            textWord.append(ch);
            break;
          }
        case TAG_NAME_START:
          {
            if (skipCommentsOrWhitespace()) {
              break;
            } else if (ch == '[' || ch == ']') {
              throw error(String.format("unexpected '%c'", ch));
            } else if (ch == ILLEGAL_QUOTE) {
              throw error("single-quotes are illegal");
            }

            // Start the tag name.
            textWord.append(ch);
            tagNamePos = pos();
            state = State.TAG_NAME_WORD;
            break;
          }
        case TAG_NAME_WORD:
          {
            if (skipCommentsOrWhitespace()) {
              tagName = textWord.toString();
              textWord = new StringBuilder();
              state = State.TAG_CLOSE_NO_ARGS;
              break;
            } else if (ch == '[') {
              throw error("unexpected '['");
            } else if (ch == ']') {
              tagName = textWord.toString();
              textWord = new StringBuilder();
              closeTagStartText();
              break;
            } else if (ch == ':') {
              // Expect arguments.
              tagName = textWord.toString();
              textWord = new StringBuilder();
              state = State.TAG_ARG_START;
              break;
            } else {
              if (ch == ILLEGAL_QUOTE) throw error("single-quotes are illegal");
            }

            // Grow the name.
            textWord.append(ch);
            break;
          }
        case TAG_CLOSE_NO_ARGS:
          {
            if (skipCommentsOrWhitespace()) {
              break;
            } else if (ch == '[') {
              throw error("Unexpected '['");
            } else if (ch == ']') {
              closeTagStartText();
              break;
            }

            throw error("unexpected tag argument: Did you forget ':'?");
          }
        case TAG_ARG_START:
          {
            if (skipCommentsOrWhitespace()) {
              break;
            } else if (ch == '[') {
              throw error("unexpected '['");
            } else if (ch == ']') {
              if (tagArgs.isEmpty()) throw error(String.format("tag closed without arguments"));

              closeTagStartText();
              break;
            } else if (ch == ILLEGAL_QUOTE) {
              throw error("single-quotes are illegal");
            }

            // Start a tag arg.
            textWord.append(ch);
            tagArgPos = pos();
            state = ch == QUOTE ? State.TAG_ARG_QUOTE : State.TAG_ARG_WORD;
            break;
          }
        case TAG_ARG_WORD:
          {
            if (skipCommentsOrWhitespace()) {
              closeTagArg();
              state = State.TAG_ARG_START;
              break;
            }

            if (ch == '[') {
              throw error("unexpected '['");
            } else if (ch == QUOTE) {
              closeTagArg();
              tagArgPos = pos();
              state = State.TAG_ARG_QUOTE;
            } else if (ch == ']') {
              closeTagArg();
              closeTagStartText();
              break;
            } else if (ch == ILLEGAL_QUOTE) {
              throw error("single-quotes are illegal");
            }

            // Grow the word.
            textWord.append(ch);
            break;
          }
        case TAG_ARG_QUOTE:
          {
            if (readEscapeChar(true)) break;

            textWord.append(ch);
            if (ch == QUOTE) {
              closeTagArg();
              state = State.TAG_ARG_START;
            }
            break;
          }
      }
    }

    if (state.ordinal() > State.TEXT_WORD.ordinal()) {
      throw new CompilerException(
          new Pos(file, lines.size() - 1, lines.get(lines.size() - 1).length() - 1),
          "got unexpected EOF: unfinished tag");
    }
    closeTextStartTag();

    ImmutableList<TagOrText> list = tagOrTextsBuilder.build();
    int lastNonWhitespace = list.size() - 1;
    while (lastNonWhitespace >= 0 && list.get(lastNonWhitespace).isWhitespace()) {
      lastNonWhitespace--;
    }

    return list.subList(0, lastNonWhitespace + 1);
  }

  private CompilerException error(String msg) {
    return new CompilerException(pos(), msg);
  }

  private boolean canPeek() {
    return canPeek(1);
  }

  private boolean canPeek(int ahead) {
    if (line >= lines.size()) {
      return false;
    }

    int nCol = col + ahead;
    int nLine = line;
    while (nCol >= lines.get(nLine).length()) {
      nCol -= lines.get(nLine).length();
      if (++nLine == lines.size()) return false;
    }
    return true;
  }

  private char peek() {
    return peek(1);
  }

  private char peek(int ahead) {
    int nCol = col + ahead;
    int nLine = line;
    while (nCol >= lines.get(nLine).length()) {
      nCol -= lines.get(nLine++).length();
    }
    return lines.get(nLine).charAt(nCol);
  }

  private boolean advance() {
    return advance(1);
  }

  private boolean advance(int ahead) {
    if (line >= lines.size()) return false;

    col += ahead;
    while (col >= lines.get(line).length()) {
      col -= lines.get(line).length();
      if (++line == lines.size()) return false;
    }

    ch = lines.get(line).charAt(col);
    return true;
  }

  private Pos pos() {
    return new Pos(file, line, col);
  }

  private Optional<Whitespace> readCommentsOrWhitespaceLoop() {
    Optional<Whitespace> result = readCommentsOrWhitespaceOnce();
    Optional<Whitespace> last = result;
    while (last.isPresent() && canPeek()) {
      int oldCol = col;
      int oldLine = line;
      char oldCh = ch;
      advance();

      last = readCommentsOrWhitespaceOnce();
      if (!last.isPresent()) {
        col = oldCol;
        line = oldLine;
        ch = oldCh;
      }
    }
    return result;
  }

  private Optional<Whitespace> readCommentsOrWhitespaceOnce() {
    Whitespace whitespace = Whitespace.create(pos());
    if (ch == '\n' || Character.isWhitespace(ch)) {
      while (canPeek() && Character.isWhitespace(peek())) advance();
      return Optional.of(whitespace);
    } else if (ch != '/' || !canPeek()) {
      return Optional.empty();
    }

    char second = peek();
    if (second == '/') {
      // Advance to the next line.
      col = -1;
      ++line;
      return Optional.of(whitespace);
    } else if (second == '*') {
      // Advance until we find the closure.
      advance();
      while (advance()) {
        if (ch == '*' && canPeek() && peek() == '/') {
          advance();
          return Optional.of(whitespace);
        }
      }

      return Optional.of(whitespace);
    } else {
      return Optional.empty();
    }
  }

  private boolean skipCommentsOrWhitespace() {
    return readCommentsOrWhitespaceLoop().isPresent();
  }

  private boolean readEscapeChar(boolean preserveQuoteEscape) throws CompilerException {
    if (ch != '\\') {
      return false;
    } else if (!canPeek()
        || (peek() != QUOTE && peek() != '\\' && peek() != 'n' && peek() != '[' && peek() != ']')) {
      throw error("illegal escape");
    }

    advance();

    if (preserveQuoteEscape && ch == '\"') {
      textWord.append("\\\"");
    } else if (ch == 'n') {
      textWord.append('\n');
    } else {
      textWord.append(ch);
    }
    return true;
  }

  private void closeTextStartTag() throws CompilerException {
    if (!textWord.toString().isEmpty()) {
      textWords.add(textWord.toString());
      textWord = new StringBuilder();
    }
    if (!textWords.isEmpty()) {
      tagOrTextsBuilder.add(
          TagOrText.text(
              Text.create(textWords.stream().collect(Collectors.joining(" ")), textPos)));
      textWords.clear();
    }
    if (trailingWhitespace.isPresent()) {
      tagOrTextsBuilder.add(TagOrText.whitespace(trailingWhitespace.get()));
      trailingWhitespace = Optional.empty();
    }
    state = State.TAG_NAME_START;
  }

  private void closeTagArg() {
    if (!textWord.toString().isEmpty()) {
      tagArgs.add(new TagArg(textWord.toString(), tagArgPos));
    }

    textWord = new StringBuilder();
    tagArgPos = null;
  }

  private void closeTagStartText() throws CompilerException {
    Tag tag = Tag.create(tagName, tagNamePos, tagArgs);
    tagOrTextsBuilder.add(TagOrText.tag(tag));

    tagName = "";
    tagNamePos = null;
    tagArgs.clear();

    state = State.TEXT;
  }
}
