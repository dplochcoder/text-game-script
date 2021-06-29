package tgs;

import java.util.Map;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;

public class ASTValidator extends ErrorCollectingValidator {

  private final ImmutableMap<String, AST> asts;

  private final LabelRegistry labelRegistry = new LabelRegistry();
  private boolean labelRegistryValid = false;
  private boolean labelRegistryFilled = false;

  public ASTValidator(Map<String, AST> asts) {
    this.asts = ImmutableMap.copyOf(asts);
  }

  public boolean fillLabelRegistry() {
    if (labelRegistryFilled) return labelRegistryValid;

    labelRegistryFilled = true;
    if (acceptAll(labelRegistry)) {
      StructDefinitionResolver structResolver = new StructDefinitionResolver(labelRegistry);
      if (acceptAll(structResolver)) {
        if (structResolver.resolveStructTypes()) {
          if (acceptAll(new ExpressionResolver(labelRegistry))
              && acceptAll(new CustomTagResolver(labelRegistry))
              && acceptAll(new LabelValidator(labelRegistry))) {
            labelRegistryValid = true;
          }
        } else {
          takeErrors(structResolver);
        }
      }
    }
    return labelRegistryValid;
  }

  public boolean hasValidLabelRegistry() {
    return labelRegistryValid;
  }

  public LabelRegistry labelRegistry() {
    return labelRegistry;
  }

  public ImmutableList<CompilerException> computeErrors() {
    if (!fillLabelRegistry()) return errors();

    acceptAll(new ExpressionTypeValidator(labelRegistry));
    acceptAll(new SwitchValidator(labelRegistry));
    acceptAll(new NoTextValidator());
    acceptAll(new DestinationTagsValidator());

    SubBlockRecursionDetector detector = new SubBlockRecursionDetector();
    acceptAll(detector);
    detector.validate(labelRegistry);
    takeErrors(detector);

    return errors();
  }

  private boolean acceptAll(ErrorCollectingValidator visitor) {
    asts.values().stream().forEach(ast -> ast.accept(visitor, null));
    takeErrors(visitor);
    return !visitor.hasErrors();
  }
}
