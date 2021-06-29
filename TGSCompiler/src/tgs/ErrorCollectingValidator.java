package tgs;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;

import com.google.common.collect.ImmutableList;
import com.google.common.graph.Graph;
import com.google.common.graph.Graphs;

abstract class ErrorCollectingValidator extends VoidDefaultASTVisitor {
  private final List<CompilerException> errors = new ArrayList<>();

  protected ImmutableList<CompilerException> errors() {
    return ImmutableList.copyOf(errors);
  }

  protected void logError(Tokenizer.Pos pos, String msg) {
    logError(new CompilerException(pos, msg));
  }

  protected void logError(CompilerException ex) {
    errors.add(ex);
  }

  protected void logTagConflict(List<? extends TypedTag> conflicts, String tagTypes, String scope) {
    if (conflicts.size() > 1) {
      conflicts.sort(Comparator.comparing(TypedTag::tagNamePos));
      logError(
          conflicts.get(1).tagNamePos(),
          String.format("found duplicate %s tag in the same %s", tagTypes, scope));
      logError(conflicts.get(0).tagNamePos(), String.format("previous %s tag is here", tagTypes));
    }
  }

  // Returns true if cycles were detected.
  protected <T> boolean detectCycles(Graph<T> graph, Consumer<T> logError) {
    Graph<T> closure = Graphs.transitiveClosure(graph);
    Set<T> logged = new HashSet<>();
    for (T node : closure.nodes()) {
      if (graph.successors(node).contains(node)) {
        if (logged.add(node)) {
          logError.accept(node);
        }
      } else {
        for (T node2 : closure.successors(node)) {
          if (!node2.equals(node) && closure.successors(node2).contains(node)) {
            if (logged.add(node)) {
              logError.accept(node);
            }
            break;
          }
        }
      }
    }

    return !logged.isEmpty();
  }

  protected void takeErrors(ErrorCollectingValidator other) {
    errors.addAll(other.errors);
  }

  public boolean hasErrors() {
    return !errors.isEmpty();
  }

  public void printErrors() {
    errors.stream().forEach(CompilerException::print);
  }
}
