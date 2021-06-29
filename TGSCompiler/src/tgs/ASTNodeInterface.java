package tgs;

public interface ASTNodeInterface {
  <V> V accept(ASTVisitor<V> visitor, V value);

  <V> V visitChildren(ASTVisitor<V> visitor, V value);
}
