package tgs;

public class CompilerException extends Exception {
  private static final long serialVersionUID = 1L;

  private final Tokenizer.Pos pos;
  private final String errorMsg;

  public CompilerException(Tokenizer.Pos pos, String errorMsg) {
    super(errorMsg);
    this.pos = pos;
    this.errorMsg = errorMsg;
  }

  public void print() {
    System.out.println(
        String.format(
            "ERROR: %s@%d:%d %s", pos.file(), pos.lineNumber() + 1, pos.column() + 1, errorMsg));
  }
}
