package tgs;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/* ASTVisitor which considers special logic for if-else branch paths as well as Prompt options. */
public abstract class ASTBranchPathVisitor<V> extends DefaultASTVisitor<V> {

  public abstract V identity();

  public abstract V reduce(V value1, V value2);

  /**
   * Combine various branch paths into a single representation. Be wary of combinatorial explosions.
   */
  public abstract V reduceBranches(V value, List<V> branches);

  @Override
  public final V visit(AST.Conditional conditional, V value) {
    List<V> branches = new ArrayList<>();
    conditional.branches().forEach(b -> branches.add(visit(b, identity())));
    if (!conditional.hasElseBranch()) branches.add(identity());

    return reduceBranches(value, branches);
  }

  @Override
  public final V visit(AST.Switch switchAst, V value) {
    List<V> cases = new ArrayList<>();
    switchAst.allCases().forEach(c -> cases.add(visit(c, identity())));

    return reduceBranches(value, cases);
  }

  @Override
  public final V visit(AST.Prompt prompt, V value) {
    value = prompt.tag().accept(this, value);
    value = prompt.promptContent().accept(this, value);

    return reduceBranches(
        value,
        prompt
            .options()
            .stream()
            .flatMap(this::mapOption)
            .collect(Collectors.toCollection(ArrayList::new)));
  }

  private Stream<V> mapOption(AST.Prompt.OptionOrOptionGroup optionOrOptionGroup) {
    if (optionOrOptionGroup.isGroup()) {
      AST.Prompt.OptionGroup optionGroup = (AST.Prompt.OptionGroup) optionOrOptionGroup;
      return optionGroup.options().stream().flatMap(this::mapOption);
    } else {
      AST.Prompt.Option option = (AST.Prompt.Option) optionOrOptionGroup;
      return Stream.of(visit(option, identity()));
    }
  }
}
