package tgs;

import java.util.Iterator;
import java.util.Optional;
import java.util.stream.Stream;

public final class ASTNodeUtils {
  public static <V> V accept(ASTNodeInterface obj, ASTVisitor<V> visitor, V value) {
    return obj.accept(visitor, value);
  }

  public static <V> V accept(
      Iterable<? extends ASTNodeInterface> obj, ASTVisitor<V> visitor, V value) {
    for (ASTNodeInterface o : obj) {
      value = accept(o, visitor, value);
    }
    return value;
  }

  public static <V> V accept(
      Stream<? extends ASTNodeInterface> obj, ASTVisitor<V> visitor, V value) {
    for (Iterator<? extends ASTNodeInterface> iter = obj.iterator(); iter.hasNext(); ) {
      value = accept(iter.next(), visitor, value);
    }
    return value;
  }

  public static <V> V accept(
      Optional<? extends ASTNodeInterface> obj, ASTVisitor<V> visitor, V value) {
    return obj.map(o -> accept(o, visitor, value)).orElse(value);
  }

  private ASTNodeUtils() {}
}
