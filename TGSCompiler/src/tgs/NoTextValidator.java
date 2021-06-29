package tgs;

public class NoTextValidator extends ErrorCollectingValidator {
  private static class ConflictDetector extends ASTBranchPathListAccumulator<TypedTag> {
    @Override
    public State<TypedTag> visit(TypedTag.NoText tag, State<TypedTag> value) {
      return value.merge(State.of(tag));
    }
  }

  private void visitBlockImpl(AST.ContentChain content, TypedTag tag) {
    ASTBranchPathListAccumulator.State<TypedTag> conflictState =
        content.accept(new ConflictDetector(), ASTBranchPathListAccumulator.State.identity());
    ASTBranchPathListAccumulator.State<TextDetector.TextOrTextTag> textState =
        content.accept(new TextDetector(), ASTBranchPathListAccumulator.State.identity());

    logTagConflict(
        conflictState.arbitraryMaxLength(), "[no_text]", String.format("[%s]", tag.tagName()));

    // We cover most cases, but not all.
    if (!conflictState.arbitraryMaxLength().isEmpty()
        && !textState.arbitraryMaxLength().isEmpty()
        && !conflictState.possiblyEmpty()
        && !textState.possiblyEmpty()) {
      logError(
          conflictState.arbitraryMaxLength().get(0).tagNamePos(),
          String.format("found [no_text] and text in the same [%s]", tag.tagName()));
      logError(textState.arbitraryMaxLength().get(0).pos(), "text found here");
    } else if (textState.arbitraryMaxLength().isEmpty()
        && conflictState.arbitraryMaxLength().isEmpty()) {
      logError(
          tag.tagNamePos(),
          String.format("[%s] has no text, but [no_text] wasn't specified", tag.tagName()));
    }
  }

  @Override
  public void visitImpl(AST.Block block) {
    visitBlockImpl(block.content(), block.tag());
    super.visitImpl(block);
  }

  @Override
  public void visitImpl(AST.SubBlock subBlock) {
    visitBlockImpl(subBlock.content(), subBlock.tag());
    super.visitImpl(subBlock);
  }

  @Override
  public void visitImpl(AST.Prompt prompt) {
    ASTBranchPathListAccumulator.State<TypedTag> conflictState =
        prompt
            .promptContent()
            .accept(new ConflictDetector(), ASTBranchPathListAccumulator.State.identity());
    ASTBranchPathListAccumulator.State<TextDetector.TextOrTextTag> textState =
        prompt
            .promptContent()
            .accept(new TextDetector(), ASTBranchPathListAccumulator.State.identity());

    logTagConflict(conflictState.arbitraryMaxLength(), "[no_text]", "[prompt]");

    if (!conflictState.arbitraryMaxLength().isEmpty()
        && !textState.arbitraryMaxLength().isEmpty()
        && !conflictState.possiblyEmpty()
        && !textState.possiblyEmpty()) {
      logError(
          conflictState.arbitraryMaxLength().get(0).tagNamePos(),
          "found [no_text] and text in the same [prompt]");
      logError(textState.arbitraryMaxLength().get(0).pos(), "text found here");
    } else if (textState.arbitraryMaxLength().isEmpty()
        && conflictState.arbitraryMaxLength().isEmpty()) {
      logError(prompt.tag().tagNamePos(), "[prompt] has no text, but [no_text] wasn't specified");
    }

    super.visitImpl(prompt);
  }

  @Override
  public void visitImpl(AST.Prompt.Option option) {
    if (option.tag().type() == TypedTag.Type.OPTION_NONE) return;

    ASTBranchPathListAccumulator.State<TextDetector.TextOrTextTag> textState =
        option.content().accept(new TextDetector(), ASTBranchPathListAccumulator.State.identity());
    if (textState.arbitraryMaxLength().isEmpty()) {
      logError(
          option.tag().tagNamePos(),
          String.format("[%s] requires text, but none was found", option.tag().tagName()));
    }

    super.visitImpl(option);
  }

  @Override
  public void visitImpl(AST.BBCodeBlock block) {
    if (block.bbCode() != BBCode.URL) return;

    ASTBranchPathListAccumulator.State<TextDetector.TextOrTextTag> textState =
        block.content().accept(new TextDetector(), ASTBranchPathListAccumulator.State.identity());
    if (textState.arbitraryMaxLength().isEmpty()) {
      logError(block.openTag().tagNamePos(), "[url] requires text, but none was found");
    }

    super.visitImpl(block);
  }
}
