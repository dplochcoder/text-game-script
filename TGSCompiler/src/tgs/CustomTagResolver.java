package tgs;

public class CustomTagResolver extends ErrorCollectingValidator {
  private final LabelRegistry labelRegistry;

  public CustomTagResolver(LabelRegistry labelRegistry) {
    this.labelRegistry = labelRegistry;
  }

  @Override
  public void visitImpl(TypedTag.CustomTag customTag) {
    try {
      customTag.resolve(labelRegistry);
    } catch (CompilerException ex) {
      logError(ex);
    }
  }
}
