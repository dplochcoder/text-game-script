package tgs;

import java.util.Optional;

import com.google.common.collect.ImmutableSet;

public class ExpressionTypeValidator extends ErrorCollectingValidator {

  private final LabelRegistry labelRegistry;

  public ExpressionTypeValidator(LabelRegistry labelRegistry) {
    this.labelRegistry = labelRegistry;
  }

  @Override
  public void visitImpl(TypedTag.Define define) {
    try {
      define.value().constantValueType();
    } catch (CompilerException ex) {
      logError(ex);
    }
  }

  @Override
  public void visitImpl(TypedTag.Switch tag) {
    expectEnum(tag.expr());
  }

  @Override
  public void visitImpl(TypedTag.To tag) {
    if (!tag.isScript()) return;
    expectType(tag.script(), Expression.ValueType.blockIdType());
  }

  @Override
  public void visitImpl(TypedTag.UsePrompt tag) {
    if (!tag.isScript()) return;
    expectType(tag.script(), Expression.ValueType.promptIdType());
  }

  @Override
  public void visitImpl(TypedTag.If ifTag) {
    expectBooleanNoSideEffects(ifTag, ifTag.expr());
  }

  @Override
  public void visitImpl(TypedTag.ElseIf elseIf) {
    expectBooleanNoSideEffects(elseIf, elseIf.expr());
  }

  @Override
  public void visitImpl(TypedTag.OptionGroupIf optionGroupIf) {
    expectBooleanNoSideEffects(optionGroupIf, optionGroupIf.expr());
  }

  @Override
  public void visitImpl(TypedTag.OptionIf optionIf) {
    expectBooleanNoSideEffects(optionIf, optionIf.expr());
  }

  private static final ImmutableSet<Expression.ValueType.TypeLiteralType> SCRIPT_TAG_LITERALS =
      ImmutableSet.of(
          Expression.ValueType.stringType().asTypeLiteral(),
          Expression.ValueType.voidType().asTypeLiteral());

  @Override
  public void visitImpl(TypedTag.ScriptTag tag) {
    TypedTag.ExternScript scriptDef = labelRegistry.scriptDefinition(tag.scriptName()).get();
    if (!SCRIPT_TAG_LITERALS.contains(scriptDef.returnType().literalType())) {
      logError(
          tag.argPos(1),
          String.format(
              "script tag function '%s' must return type STRING or VOID", tag.scriptName()));
      logError(
          scriptDef.returnType().pos(),
          String.format(
              "extern_script '%s' defined here with type %s",
              tag.scriptName(), scriptDef.returnType()));
    }
  }

  @Override
  public void visitImpl(TypedTag.StringTag tag) {
    expectType(tag.expr(), Expression.ValueType.stringType());
  }

  @Override
  public void visitImpl(TypedTag.Use use) {
    expectType(use.key(), StructType.key().valueType());
    if (use.hasScript()) expectType(use.script(), Expression.ValueType.blockIdType());
  }

  @Override
  public void visitImpl(TypedTag.Do doTag) {
    try {
      doTag.expr().valueType(labelRegistry);
    } catch (CompilerException ex) {
      logError(ex);
      return;
    }

    if (doTag.expr().type() != Expression.Type.SCRIPT && !isSideEffecting(doTag.expr())) {
      logError(doTag.tagNamePos(), "expected one of '$script', '++', '--', '=', '+=' or '-='");
    }

    Optional<Tokenizer.Pos> pos =
        doTag.expr().accept(new IllegalSideEffectsAnalyzer(), Optional.empty());
    if (pos.isPresent()) {
      logError(
          pos.get(),
          "mutational operations not allowed on the left side of an assignment operator");
    }
  }

  @Override
  public void visitImpl(TypedTag.ColorOpen colorOpen) {
    if (!colorOpen.hasHexLiteral())
      expectType(colorOpen.variable(), Expression.ValueType.hexColorType());
  }

  @Override
  public void visitImpl(TypedTag.ExternScript externScript) {
    // Don't validate these - they're validated during resolve with different logic.
  }

  private void expectType(Expression expr, Expression.ValueType type) {
    try {
      Expression.ValueType actualType = expr.valueType(labelRegistry);
      if (!type.equals(actualType))
        logError(expr.pos(), String.format("expected %s here, but was %s", type, actualType));
    } catch (CompilerException ex) {
      logError(ex);
    }
  }

  private void expectEnum(Expression expr) {
    try {
      Expression.ValueType actualType = expr.valueType(labelRegistry);
      if (!actualType.isEnum())
        logError(expr.pos(), String.format("expected enum value here, but was %s", actualType));
    } catch (CompilerException ex) {
      logError(ex);
    }
  }

  private void expectBooleanNoSideEffects(TypedTag tag, Expression expr) {
    expectType(expr, Expression.ValueType.booleanType());

    SideEffectsAnalyzer analyzer = new SideEffectsAnalyzer();
    Optional<Tokenizer.Pos> sideEffectPos = expr.accept(analyzer, Optional.empty());
    sideEffectPos.ifPresent(
        p -> logError(p, "mutational operations are not allowed in conditional expressions"));
  }

  private static class SideEffectsAnalyzer extends DefaultASTVisitor<Optional<Tokenizer.Pos>> {
    @Override
    public Optional<Tokenizer.Pos> visit(Expression.Unary unary, Optional<Tokenizer.Pos> value) {
      if (value.isPresent()) return value;

      if (isSideEffecting(unary)) return Optional.of(unary.opPos());
      else return super.visit(unary, value);
    }

    @Override
    public Optional<Tokenizer.Pos> visit(Expression.Binary binary, Optional<Tokenizer.Pos> value) {
      if (value.isPresent()) return value;

      if (isSideEffecting(binary)) return Optional.of(binary.opPos());
      else return super.visit(binary, value);
    }
  }

  private static boolean isSideEffecting(Expression expr) {
    switch (expr.type()) {
      case UNARY:
        Expression.Unary unary = expr.cast();
        switch (unary.op()) {
          case INCREMENT:
          case DECREMENT:
            return true;
          default:
            return false;
        }
      case BINARY:
        Expression.Binary binary = expr.cast();
        switch (binary.op()) {
          case ASSIGNMENT:
          case PLUS_EQUALS:
          case MINUS_EQUALS:
            return true;
          default:
            return false;
        }
      default:
        return false;
    }
  }

  private static class IllegalSideEffectsAnalyzer
      extends DefaultASTVisitor<Optional<Tokenizer.Pos>> {
    @Override
    public Optional<Tokenizer.Pos> visit(Expression.Binary binary, Optional<Tokenizer.Pos> value) {
      if (value.isPresent()) return value;

      if (isSideEffecting(binary)) return binary.lhs().accept(new SideEffectsAnalyzer(), value);
      else return super.visit(binary, value);
    }
  }
}
