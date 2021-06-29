package tgs;

import java.util.HashSet;
import java.util.Set;

public class ExpressionResolver extends ErrorCollectingValidator {
  private final LabelRegistry labelRegistry;

  public ExpressionResolver(LabelRegistry labelRegistry) {
    this.labelRegistry = labelRegistry;
  }

  @Override
  public void visitImpl(Expression.Holder<?> holder) {
    try {
      Expression resolved = holder.get().resolve(labelRegistry);
      holder.set(resolved.cast());
    } catch (CompilerException ex) {
      logError(ex);
    }
  }

  @Override
  public void visitImpl(TypedTag.ExternScript externScript) {
    super.visitImpl(externScript);

    Expression expr = externScript.rawExpression();
    if (expr.type() != Expression.Type.TYPE_ANNOTATED_EXPRESSION) {
      logError(expr.pos(), "expected a $script:return_type declaration here");
      return;
    }

    Expression.TypeAnnotatedExpression top = expr.cast();
    if (top.expression().type() != Expression.Type.SCRIPT) {
      logError(expr.pos(), "expected a $script here");
      return;
    }

    Expression.Script script = top.expression().cast();
    Set<String> localVars = new HashSet<>();
    for (int i = 0; i < script.numArgs(); i++) {
      Expression arg = script.arg(i);
      if (arg.type() != Expression.Type.TYPE_ANNOTATED_EXPRESSION) {
        logError(arg.pos(), "expected a param:type declaration here");
        continue;
      }

      Expression.TypeAnnotatedExpression argTyped = arg.cast();
      if (argTyped.expression().type() != Expression.Type.VARIABLE) {
        logError(arg.pos(), "expected a variable here");
        continue;
      }

      Expression.Variable var = argTyped.expression().cast();
      if (labelRegistry.isVariableDefined(var.name()))
        logError(var.pos(), "local script parameter name shadows a global variable");

      if (!localVars.add(var.name())) logError(var.pos(), "duplicate local script parameter name");
    }
  }
}
