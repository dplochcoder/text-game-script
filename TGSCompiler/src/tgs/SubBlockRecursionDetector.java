package tgs;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.google.common.collect.Sets;
import com.google.common.graph.GraphBuilder;
import com.google.common.graph.MutableGraph;

public class SubBlockRecursionDetector extends ErrorCollectingValidator {
  private static class SubBlockVisitor extends ASTBranchPathVisitor<Set<String>> {
    @Override
    public Set<String> identity() {
      return new HashSet<>();
    }

    @Override
    public Set<String> reduce(Set<String> value1, Set<String> value2) {
      value1.addAll(value2);
      return value1;
    }

    @Override
    public Set<String> reduceBranches(Set<String> value, List<Set<String>> branches) {
      value.addAll(
          branches
              .stream()
              .reduce(new HashSet<>(), (a, b) -> new HashSet<>(Sets.intersection(a, b))));
      return value;
    }

    @Override
    public Set<String> visit(TypedTag.UseSubBlock tag, Set<String> value) {
      value.add(tag.id().name());
      return value;
    }
  }

  MutableGraph<String> edges = GraphBuilder.directed().allowsSelfLoops(true).build();

  @Override
  public void visitImpl(AST.SubBlock subBlock) {
    edges.addNode(subBlock.id().name());

    for (String target : subBlock.content().accept(new SubBlockVisitor(), new HashSet<>())) {
      edges.putEdge(subBlock.id().name(), target);
    }
  }

  public boolean validate(LabelRegistry registry) {
    return !detectCycles(
        edges,
        node ->
            logError(
                registry.getSubBlock(node).get().tag().tagNamePos(),
                String.format("sub_block '%s' is fully recursive and will never terminate", node)));
  }
}
