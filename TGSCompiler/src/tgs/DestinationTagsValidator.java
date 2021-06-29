package tgs;

import com.google.common.collect.HashMultiset;
import com.google.common.collect.Multiset;

public final class DestinationTagsValidator extends ErrorCollectingValidator {
  private static final class DestinationConflictDetector
      extends ASTBranchPathListAccumulator<TypedTag> {
    @Override
    public State<TypedTag> visit(TypedTag.To tag, State<TypedTag> value) {
      return value.merge(State.of(tag));
    }

    @Override
    public State<TypedTag> visit(TypedTag.UsePrompt tag, State<TypedTag> value) {
      return value.merge(State.of(tag));
    }

    @Override
    public State<TypedTag> visit(TypedTag.Prompt tag, State<TypedTag> value) {
      return value.merge(State.of(tag));
    }

    @Override
    public State<TypedTag> visit(AST.Prompt.Option option, State<TypedTag> value) {
      // Ignore prompt options to avoid error spam.
      return value;
    }
  }

  private static final class KeyAccumulator extends DefaultASTVisitor<Multiset<String>> {
    @Override
    public Multiset<String> visit(TypedTag.Use use, Multiset<String> value) {
      value.add(use.key().name());
      return value;
    }
  }

  private static final class KeyConflictDetector
      extends ASTBranchPathListAccumulator<TypedTag.Use> {
    private final String keyName;

    public KeyConflictDetector(String keyName) {
      this.keyName = keyName;
    }

    @Override
    public State<TypedTag.Use> visit(TypedTag.Use use, State<TypedTag.Use> value) {
      if (use.key().name().equals(keyName)) value.merge(State.of(use));
      return value;
    }
  }

  @Override
  public void visitImpl(AST.Block block) {
    logTagConflict(
        block
            .accept(
                new DestinationConflictDetector(), ASTBranchPathListAccumulator.State.identity())
            .arbitraryMaxLength(),
        "[auto_next|auto_to|to|prompt|use_prompt]",
        "[block]");

    // Check for key-name conflicts within the block.
    Multiset<String> counts = block.content().accept(new KeyAccumulator(), HashMultiset.create());
    for (Multiset.Entry<String> entry : counts.entrySet()) {
      if (entry.getCount() <= 1) continue;

      logTagConflict(
          block
              .content()
              .accept(
                  new KeyConflictDetector(entry.getElement()),
                  ASTBranchPathListAccumulator.State.identity())
              .arbitraryMaxLength(),
          String.format("[use: %s ...]", entry.getElement()),
          "[block]");
    }
  }

  @Override
  public void visitImpl(AST.Prompt.Option option) {
    ASTBranchPathListAccumulator.State<TypedTag> state =
        option
            .content()
            .accept(
                new DestinationConflictDetector(), ASTBranchPathListAccumulator.State.identity());

    logTagConflict(state.arbitraryMaxLength(), "[auto_next|auto_to|to]", "[option]");
    if (state.possiblyEmpty())
      logError(
          option.tag().tagNamePos(),
          String.format("[%s] tags require [to: ...] or [auto_to: ...]", option.tag().tagName()));
  }
}
