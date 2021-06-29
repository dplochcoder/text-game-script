package tgs;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

import com.google.common.collect.HashMultiset;
import com.google.common.collect.Multiset;
import com.google.common.graph.GraphBuilder;
import com.google.common.graph.MutableGraph;

import tgs.Expression.Variable;

public class StructDefinitionResolver extends ErrorCollectingValidator {

  private static class StructDependencyCollector extends DefaultASTVisitor<Set<String>> {
    private static final StructDependencyCollector INSTANCE = new StructDependencyCollector();

    public static StructDependencyCollector instance() {
      return INSTANCE;
    }

    @Override
    public Set<String> visit(Expression.Binary binary, Set<String> value) {
      value = super.visit(binary, value);

      // Check for unresolved struct literals.
      if (binary.op() == Expression.BinaryOperator.MEMBER_REFERENCE
          && binary.lhs().type() == Expression.Type.VARIABLE
          && binary.rhs().type() == Expression.Type.VARIABLE) {
        Variable left = binary.lhs().cast();
        Variable right = binary.rhs().cast();
        if (left.name().equals("STRUCT")) {
          value.add(right.name());
        }
      }
      return value;
    }
  }

  private final LabelRegistry labelRegistry;
  private final MutableGraph<String> dependencies =
      GraphBuilder.directed().allowsSelfLoops(true).build();

  public StructDefinitionResolver(LabelRegistry labelRegistry) {
    this.labelRegistry = labelRegistry;
  }

  @Override
  public void visitImpl(AST.StructDefinition structDefinition) {
    String node = structDefinition.typeName();
    dependencies.addNode(node);

    StructDependencyCollector.instance()
        .visit(structDefinition, new HashSet<>())
        .forEach(n -> dependencies.putEdge(n, node));
  }

  public boolean resolveStructTypes() {
    if (detectCycles(
        dependencies,
        node ->
            logError(
                labelRegistry.getStructDefinition(node).get().tag().tagNamePos(),
                String.format("STRUCT.%s is self-referential and cannot be instantiated", node)))) {
      return false;
    }

    /// Do a topological sort.
    Multiset<String> depCounts = HashMultiset.create();
    Deque<String> sources = new ArrayDeque<>();
    for (String node : dependencies.nodes()) {
      int deps = dependencies.inDegree(node);
      if (deps == 0) {
        sources.add(node);
      } else {
        depCounts.add(node, deps);
      }
    }

    while (!sources.isEmpty()) {
      String next = sources.pollFirst();
      dependencies
          .successors(next)
          .stream()
          .filter(n -> depCounts.remove(n, 1) == 1)
          .forEach(sources::add);

      Optional<AST.StructDefinition> def = labelRegistry.getStructDefinition(next);
      if (def.isPresent()) {
        try {
          labelRegistry.addStructType(def.get().defineStructType(labelRegistry));
        } catch (CompilerException ex) {
          logError(ex);
        }
      }
    }

    return !hasErrors();
  }
}
