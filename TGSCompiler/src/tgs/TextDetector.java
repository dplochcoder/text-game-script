package tgs;

import java.util.Optional;

public class TextDetector extends ASTBranchPathListAccumulator<TextDetector.TextOrTextTag> {
  public static class TextOrTextTag {
    private final Optional<Tokenizer.Text> text;
    private final Optional<TypedTag> textTag;

    private TextOrTextTag(Optional<Tokenizer.Text> text, Optional<TypedTag> textTag) {
      this.text = text;
      this.textTag = textTag;
    }

    public static TextOrTextTag text(Tokenizer.Text text) {
      return new TextOrTextTag(Optional.of(text), Optional.empty());
    }

    public static TextOrTextTag textTag(TypedTag textTag) {
      return new TextOrTextTag(Optional.empty(), Optional.of(textTag));
    }

    public Tokenizer.Pos pos() {
      return text.map(t -> t.startPos()).orElseGet(() -> textTag.get().tagNamePos());
    }
  }

  @Override
  public State<TextOrTextTag> visit(Tokenizer.Text text, State<TextOrTextTag> value) {
    return value.merge(State.of(TextOrTextTag.text(text)));
  }

  @Override
  public State<TextOrTextTag> visit(TypedTag.StringTag tag, State<TextOrTextTag> value) {
    return value.merge(State.of(TextOrTextTag.textTag(tag)));
  }
}
