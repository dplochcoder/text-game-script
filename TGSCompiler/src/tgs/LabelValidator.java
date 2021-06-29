package tgs;

class LabelValidator extends ErrorCollectingValidator {

  private final LabelRegistry labelRegistry;

  public LabelValidator(LabelRegistry labelRegistry) {
    this.labelRegistry = labelRegistry;
  }

  @Override
  public void visitImpl(Expression.BlockId blockId) {
    if (!labelRegistry.isBlockDefined(blockId.name()))
      logError(blockId.pos(), "undefined block id: " + blockId.name());
  }

  @Override
  public void visitImpl(Expression.SubBlockId subBlockId) {
    if (!labelRegistry.getSubBlock(subBlockId.name()).isPresent())
      logError(subBlockId.pos(), "undefined sub block id: " + subBlockId.name());
  }

  @Override
  public void visitImpl(Expression.PromptId promptId) {
    if (!labelRegistry.getPrompt(promptId.name()).isPresent())
      logError(promptId.pos(), "undefined prompt id: " + promptId.name());
  }

  @Override
  public void visitImpl(Expression.Variable variable) {
    if (!labelRegistry.isVariableDefined(variable.name()))
      logError(variable.pos(), "undefined variable: " + variable.name());
  }

  @Override
  public void visitImpl(TypedTag.CustomTag tag) {
    if (!labelRegistry.scriptTagDefinition(tag.tagName()).isPresent())
      logError(tag.tagNamePos(), "unrecognized tag name: " + tag.tagName());
  }

  @Override
  public void visitImpl(TypedTag.ExternScript tag) {
    // Don't validate these variables.
  }

  @Override
  public void visitImpl(TypedTag.ScriptTag tag) {
    if (!labelRegistry.scriptDefinition(tag.scriptName()).isPresent())
      logError(tag.argPos(1), String.format("no extern_script named '%s'", tag.scriptName()));
  }
}
