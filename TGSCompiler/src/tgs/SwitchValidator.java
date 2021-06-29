package tgs;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Sets;

public class SwitchValidator extends ErrorCollectingValidator {

  private final LabelRegistry labelRegistry;

  public SwitchValidator(LabelRegistry labelRegistry) {
    this.labelRegistry = labelRegistry;
  }

  @Override
  public void visitImpl(AST.Switch switchAst) {
    super.visitImpl(switchAst);

    TypedTag.DefineEnum defineEnum;
    try {
      Expression.ValueType.EnumValueType enumType =
          switchAst.switchTag().expr().valueType(labelRegistry).cast();
      defineEnum = enumType.enumType();
    } catch (CompilerException ex) {
      logError(ex);
      return;
    } catch (Exception ex) {
      return;
    }

    Map<String, Tokenizer.Pos> casePos = new HashMap<>();
    for (AST.Switch.Case caseAst : switchAst.nonDefaultCases()) {
      for (Tokenizer.TagArg arg : caseAst.caseTag().rawArgs()) {
        if (!defineEnum.values().contains(arg.arg())) {
          logError(arg.pos(), String.format("Not a valid ENUM.%s value", defineEnum.typeName()));
        } else if (!casePos.containsKey(arg.arg())) {
          casePos.put(arg.arg(), arg.pos());
        } else {
          logError(arg.pos(), "Duplicate enum case");
          logError(casePos.get(arg.arg()), "Duplicate enum case");
        }
      }
    }

    ImmutableList<String> missing =
        Sets.difference(defineEnum.values(), casePos.keySet())
            .stream()
            .sorted()
            .collect(ImmutableList.toImmutableList());
    if (missing.isEmpty() && switchAst.hasDefaultCase()) {
      logError(
          switchAst.defaultCase().defaultTag().tagNamePos(),
          "[default] case is inaccessible: all values are covered by other [case] tags");
    } else if (!missing.isEmpty() && !switchAst.hasDefaultCase()) {
      logError(
          switchAst.switchTag().tagNamePos(),
          String.format(
              "[switch] is missing [case] statements or a [default] statement for values: %s",
              missing.stream().collect(Collectors.joining(", "))));
    }
  }
}
