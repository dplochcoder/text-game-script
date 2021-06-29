package tgs;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.google.auto.value.AutoValue;
import com.google.common.base.Preconditions;
import com.google.common.base.Verify;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterables;
import com.google.common.collect.Maps;
import com.google.common.io.ByteArrayDataOutput;

import tgs.Compiler.Registry;
import tgs.processor.ASTChild;
import tgs.processor.ASTNode;

// AST and infix parser for arbitrary boolean expressions
public abstract class Expression implements ASTNodeInterface {

  public enum Type {
    // Intermediary operator nodes.
    // These don't exist in the final node hierarchy when parsing is complete.
    PARENTHESIS,
    COMMA,
    SCRIPT_REFERENCE,
    TUPLE,
    UNARY_OPERATOR,
    BINARY_OPERATOR,

    // Type annotation components
    TYPE_LITERAL,
    TYPE_ANNOTATED_EXPRESSION,

    // Value atoms
    BOOLEAN_CONSTANT,
    INTEGER_CONSTANT,
    DOUBLE_CONSTANT,
    STRING_LITERAL,
    HEX_COLOR_LITERAL,
    VARIABLE,
    ENUM_VALUE_LITERAL,
    UNRESOLVED_CALL_OPERATION,
    STRUCT_INSTANCE,
    STRUCT_MEMBER_REFERENCE,
    BLOCK_ID,
    SUB_BLOCK_ID,
    PROMPT_ID,
    SCRIPT,

    // Compounds
    UNARY,
    UNARY_NEGATION,
    BINARY;

    public boolean isOperator() {
      return this == UNARY_OPERATOR || this == BINARY_OPERATOR;
    }

    public boolean isAssignable() {
      return this == VARIABLE || this == STRUCT_MEMBER_REFERENCE;
    }
  }

  public static class ValueType {
    public enum Type {
      INTEGER,
      DOUBLE,
      BOOLEAN,
      STRING,
      HEX_COLOR,
      BLOCK_ID,
      SUB_BLOCK_ID,
      PROMPT_ID,
      VOID,
      ENUM,
      STRUCT,
      TYPE_LITERAL;
    }

    private static final ValueType INTEGER_TYPE = new ValueType(Type.INTEGER);
    private static final ValueType DOUBLE_TYPE = new ValueType(Type.DOUBLE);
    private static final ValueType BOOLEAN_TYPE = new ValueType(Type.BOOLEAN);
    private static final ValueType STRING_TYPE = new ValueType(Type.STRING);
    private static final ValueType HEX_COLOR_TYPE = new ValueType(Type.HEX_COLOR);
    private static final ValueType BLOCK_ID_TYPE = new ValueType(Type.BLOCK_ID);
    private static final ValueType SUB_BLOCK_ID_TYPE = new ValueType(Type.SUB_BLOCK_ID);
    private static final ValueType PROMPT_ID_TYPE = new ValueType(Type.PROMPT_ID);
    private static final ValueType VOID_TYPE = new ValueType(Type.VOID);

    public static ValueType integerType() {
      return INTEGER_TYPE;
    }

    public static ValueType doubleType() {
      return DOUBLE_TYPE;
    }

    public static ValueType booleanType() {
      return BOOLEAN_TYPE;
    }

    public static ValueType stringType() {
      return STRING_TYPE;
    }

    public static ValueType hexColorType() {
      return HEX_COLOR_TYPE;
    }

    public static ValueType blockIdType() {
      return BLOCK_ID_TYPE;
    }

    public static ValueType promptIdType() {
      return PROMPT_ID_TYPE;
    }

    public static ValueType voidType() {
      return VOID_TYPE;
    }

    public final boolean isInteger() {
      return type == Type.INTEGER;
    }

    public final boolean isDouble() {
      return type == Type.DOUBLE;
    }

    public final boolean isBoolean() {
      return type == Type.BOOLEAN;
    }

    public final boolean isString() {
      return type == Type.STRING;
    }

    public final boolean isHexColor() {
      return type == Type.HEX_COLOR;
    }

    public final boolean isBlockId() {
      return type == Type.BLOCK_ID;
    }

    public final boolean isPromptId() {
      return type == Type.PROMPT_ID;
    }

    public final boolean isVoid() {
      return type == Type.VOID;
    }

    public final boolean isEnum() {
      return type == Type.ENUM;
    }

    public final boolean isStruct() {
      return type == Type.STRUCT;
    }

    public final boolean isTypeLiteral() {
      return type == Type.TYPE_LITERAL;
    }

    private final Type type;
    private TypeLiteralType literalType = null;

    private ValueType(Type type) {
      this.type = type;
    }

    public final Type type() {
      return type;
    }

    public TypeLiteralType asTypeLiteral() {
      if (type == Type.TYPE_LITERAL) return null;

      literalType = new TypeLiteralType(this);
      return literalType;
    }

    @SuppressWarnings("unchecked")
    public <T extends ValueType> T cast() {
      return (T) this;
    }

    public final boolean isNumeric() {
      return type == Type.INTEGER || type == Type.DOUBLE;
    }

    public final boolean supportsEquals() {
      return type == Type.INTEGER
          || type == Type.BOOLEAN
          || type == Type.STRING
          || type == Type.HEX_COLOR
          || type == Type.ENUM;
    }

    public final boolean supportsComparison() {
      return type == Type.INTEGER || type == Type.DOUBLE || type == Type.ENUM;
    }

    public final boolean canCoerce(ValueType that) {
      return this.equals(that) || (isDouble() && that.isInteger());
    }

    @Override
    public boolean equals(Object o) {
      if (!(o instanceof ValueType)) return false;

      ValueType that = (ValueType) o;
      return this.type == that.type;
    }

    @Override
    public int hashCode() {
      return type.hashCode();
    }

    @Override
    public String toString() {
      return type.toString();
    }

    public static final class EnumValueType extends ValueType {
      private final TypedTag.DefineEnum enumType;

      private EnumValueType(TypedTag.DefineEnum enumType) {
        super(ValueType.Type.ENUM);
        this.enumType = enumType;
      }

      public static EnumValueType of(TypedTag.DefineEnum enumType) {
        return new EnumValueType(enumType);
      }

      public TypedTag.DefineEnum enumType() {
        return enumType;
      }

      @Override
      public boolean equals(Object o) {
        if (!(o instanceof EnumValueType)) return false;

        EnumValueType that = (EnumValueType) o;
        return this.enumType.typeName().equals(that.enumType.typeName());
      }

      @Override
      public int hashCode() {
        return enumType.typeName().hashCode();
      }

      @Override
      public String toString() {
        return "ENUM." + enumType.typeName();
      }
    }

    public static final class StructValueType extends ValueType {
      private final StructType structType;

      private StructValueType(StructType structType) {
        super(ValueType.Type.STRUCT);
        this.structType = structType;
      }

      public static StructValueType of(StructType structType) {
        return new StructValueType(structType);
      }

      public StructType structType() {
        return structType;
      }

      @Override
      public boolean equals(Object o) {
        if (!(o instanceof StructValueType)) return false;

        StructValueType that = (StructValueType) o;
        return this.structType.typeName().equals(that.structType.typeName());
      }

      @Override
      public int hashCode() {
        return structType.typeName().hashCode();
      }

      @Override
      public String toString() {
        return "STRUCT." + structType.typeName();
      }
    }

    public static class TypeLiteralType extends ValueType {
      private final ValueType valueType;

      protected TypeLiteralType(ValueType valueType) {
        super(Type.TYPE_LITERAL);

        Verify.verify(!valueType.isTypeLiteral());
        this.valueType = valueType;
      }

      public final ValueType valueType() {
        return valueType;
      }

      private static final ImmutableMap<String, TypeLiteralType> ATOMIC_LITERALS_MAP =
          Stream.of(
                  ValueType.integerType(),
                  ValueType.doubleType(),
                  ValueType.stringType(),
                  ValueType.hexColorType(),
                  ValueType.booleanType(),
                  ValueType.blockIdType(),
                  ValueType.promptIdType(),
                  ValueType.voidType())
              .collect(ImmutableMap.toImmutableMap(v -> v.type().name(), v -> v.asTypeLiteral()));

      private static Optional<TypeLiteralType> parse(String atom) {
        return Optional.ofNullable(ATOMIC_LITERALS_MAP.get(atom));
      }

      @Override
      public TypeLiteralType asTypeLiteral() {
        throw new UnsupportedOperationException();
      }

      @Override
      public boolean equals(Object o) {
        if (!(o instanceof TypeLiteralType)) return false;

        TypeLiteralType that = (TypeLiteralType) o;
        return this.valueType.equals(that.valueType);
      }

      @Override
      public int hashCode() {
        return valueType.hashCode();
      }

      @Override
      public String toString() {
        return String.format("TYPE(%s)", valueType);
      }
    }
  }

  public enum UnaryOrder {
    PREFIX,
    POSTFIX;
  }

  public enum UnaryOperator {
    // Logical
    NOT("not", UnaryOrder.PREFIX, false),

    // Procedural
    INCREMENT("++", UnaryOrder.POSTFIX),
    DECREMENT("--", UnaryOrder.POSTFIX);

    private final String repr;
    private final UnaryOrder order;
    private final boolean splittable;

    UnaryOperator(String repr, UnaryOrder order) {
      this(repr, order, true);
    }

    UnaryOperator(String repr, UnaryOrder order, boolean splittable) {
      this.repr = repr;
      this.order = order;
      this.splittable = splittable;
    }

    public String repr() {
      return repr;
    }

    public UnaryOrder order() {
      return order;
    }

    private static final ImmutableMap<String, UnaryOperator> REPR_MAP =
        Arrays.asList(values())
            .stream()
            .collect(ImmutableMap.toImmutableMap(u -> u.repr(), u -> u));

    public static Optional<UnaryOperator> parse(String atom) {
      return Optional.ofNullable(REPR_MAP.get(atom));
    }

    private static final ImmutableSet<String> SPLIT_REPRS =
        Arrays.asList(values())
            .stream()
            .filter(u -> u.splittable)
            .map(u -> u.repr())
            .collect(ImmutableSet.toImmutableSet());

    public static ImmutableSet<String> splitReprs() {
      return SPLIT_REPRS;
    }
  }

  public enum BinaryOperator {
    // Programmatic
    MEMBER_REFERENCE("."),
    TYPE_ANNOTATOR(":"),

    // Logical
    AND("and", false),
    OR("or", false),
    LESS_THAN("<"),
    LESS_THAN_OR_EQUAL("<="),
    GREATER_THAN(">"),
    GREATER_THAN_OR_EQUAL(">="),
    EQUAL("=="),
    NOT_EQUAL("!="),

    // Mathematical,
    ADD("+"),
    SUBTRACT("-"),
    MULTIPLY("*"),
    DIVIDE("/"),

    // Procedural
    ASSIGNMENT("="),
    PLUS_EQUALS("+="),
    MINUS_EQUALS("-=");

    private final String repr;
    private final boolean splittable;

    BinaryOperator(String repr) {
      this(repr, true);
    }

    BinaryOperator(String repr, boolean splittable) {
      this.repr = repr;
      this.splittable = splittable;
    }

    public String repr() {
      return repr;
    }

    private static final ImmutableMap<String, BinaryOperator> REPR_MAP =
        Maps.uniqueIndex(Arrays.asList(values()), BinaryOperator::repr);

    public static Optional<BinaryOperator> parse(String atom) {
      return Optional.ofNullable(REPR_MAP.get(atom));
    }

    private static final ImmutableList<ImmutableSet<BinaryOperator>> ORDER_OF_OPERATIONS =
        ImmutableList.of(
            ImmutableSet.of(TYPE_ANNOTATOR),
            ImmutableSet.of(MULTIPLY, DIVIDE),
            ImmutableSet.of(ADD, SUBTRACT),
            ImmutableSet.of(
                LESS_THAN,
                LESS_THAN_OR_EQUAL,
                GREATER_THAN,
                GREATER_THAN_OR_EQUAL,
                EQUAL,
                NOT_EQUAL),
            ImmutableSet.of(AND, OR),
            ImmutableSet.of(ASSIGNMENT, PLUS_EQUALS, MINUS_EQUALS));

    static {
      // Ensure each operator is listed exactly once.
      // MEMBER_REFERENCE is special and is done before unary operators.
      Verify.verify(
          Arrays.asList(values())
              .stream()
              .filter(op -> op != MEMBER_REFERENCE)
              .allMatch(b -> ORDER_OF_OPERATIONS.stream().filter(s -> s.contains(b)).count() == 1));
      Verify.verify(ORDER_OF_OPERATIONS.stream().noneMatch(s -> s.contains(MEMBER_REFERENCE)));
    }

    public static ImmutableList<ImmutableSet<BinaryOperator>> orderOfOperations() {
      return ORDER_OF_OPERATIONS;
    }

    private static final ImmutableSet<String> SPLIT_REPRS =
        Arrays.asList(values())
            .stream()
            .filter(b -> b.splittable)
            .map(b -> b.repr())
            .collect(ImmutableSet.toImmutableSet());

    public static ImmutableSet<String> splitReprs() {
      return SPLIT_REPRS;
    }
  }

  // Validates a label or variable name
  public static String validateId(String id, Tokenizer.Pos pos) throws CompilerException {
    for (int i = 0; i < id.length(); i++) {
      char ch = id.charAt(i);
      if (Character.isAlphabetic(ch) || (i > 0 && (Character.isDigit(ch) || ch == '_'))) continue;

      throw new CompilerException(
          pos.addColumns(1), String.format("illegal character '%c' in id", ch));
    }

    return id;
  }

  public static String validateId(Tokenizer.TagArg arg) throws CompilerException {
    return validateId(arg.arg(), arg.pos());
  }

  // Parses a singular expression atom, no spaces.
  private static Expression parseAtom(String atom, Tokenizer.Pos pos) throws CompilerException {
    if (atom.isEmpty()) throw new CompilerException(pos, "unexpected empty string");

    Optional<Parenthesis> paren = Parenthesis.parse(atom, pos);
    if (paren.isPresent()) return paren.get();

    Optional<Comma> comma = Comma.parse(atom, pos);
    if (comma.isPresent()) return comma.get();

    Optional<UnaryOperator> unary = UnaryOperator.parse(atom);
    if (unary.isPresent()) return new UnaryOperatorAtom(unary.get(), pos);

    Optional<BinaryOperator> binary = BinaryOperator.parse(atom);
    if (binary.isPresent()) return new BinaryOperatorAtom(binary.get(), pos);

    Optional<ValueType.TypeLiteralType> typeLiteral = ValueType.TypeLiteralType.parse(atom);
    if (typeLiteral.isPresent()) return new TypeLiteral(typeLiteral.get(), pos);

    if (atom.equalsIgnoreCase("true") || atom.equalsIgnoreCase("false"))
      return new BooleanConstant(Boolean.valueOf(atom.toLowerCase()), pos);

    if (atom.startsWith("#")) return HexColorLiteral.parse(atom, pos);

    if (atom.startsWith("$")) return ScriptReference.parse(atom, pos);

    if (Character.isDigit(atom.charAt(0))) {
      if (atom.chars().skip(1).allMatch(Character::isDigit))
        return IntegerConstant.parse(atom, pos);
      else return DoubleConstant.parse(atom, pos);
    }

    if (atom.startsWith(Character.toString(Tokenizer.QUOTE))) return StringLiteral.parse(atom, pos);

    // If none of the above, it must be a Variable.
    return Variable.parse(atom, pos);
  }

  private static final ImmutableSet<String> ALL_SPLITTABLE_OP_REPRS;
  private static final ImmutableSet<String> ALL_SPLITTABLE_OP_REPR_STRICT_PREFIXES;
  private static final ImmutableSet<Character> ALL_SPLITTABLE_OP_CHARS;

  static {
    ImmutableSet.Builder<String> all = ImmutableSet.builder();
    ImmutableSet.Builder<String> prefix = ImmutableSet.builder();
    ImmutableSet.Builder<Character> chars = ImmutableSet.builder();

    for (String repr : Iterables.concat(UnaryOperator.splitReprs(), BinaryOperator.splitReprs())) {
      all.add(repr);
      for (int i = 0; i < repr.length(); i++) {
        chars.add(repr.charAt(i));
        if (i >= 1) {
          prefix.add(repr.substring(0, i));
        }
      }
    }

    ALL_SPLITTABLE_OP_REPRS = all.build();
    ALL_SPLITTABLE_OP_REPR_STRICT_PREFIXES = prefix.build();
    ALL_SPLITTABLE_OP_CHARS = chars.build();
  }

  private static Stream<Tokenizer.TagArg> splitSeparatorsAndOperators(Tokenizer.TagArg arg) {
    if (arg.arg().startsWith("\"")) {
      return Stream.of(arg);
    }

    List<Tokenizer.TagArg> splits = new ArrayList<>();
    Tokenizer.Pos startPos = arg.pos();
    StringBuilder wordBuilder = new StringBuilder();
    for (int i = 0; i < arg.arg().length(); i++) {
      char ch = arg.arg().charAt(i);
      if (ch == '(' || ch == ')' || ch == ',') {
        String word = wordBuilder.toString();
        wordBuilder = new StringBuilder();
        if (!word.isEmpty()) {
          splits.add(new Tokenizer.TagArg(word, startPos));
        }

        splits.add(
            new Tokenizer.TagArg(Character.toString(ch), startPos.addColumns(word.length())));
        startPos = startPos.addColumns(word.length() + 1);
        continue;
      }

      boolean isOp = ALL_SPLITTABLE_OP_CHARS.contains(ch);
      if (!isOp) {
        wordBuilder.append(ch);
        continue;
      }

      String word = wordBuilder.toString();
      wordBuilder = new StringBuilder();
      if (!word.isEmpty()) {
        splits.add(new Tokenizer.TagArg(word, startPos));
      }

      startPos = startPos.addColumns(word.length());

      // Do a look-ahead to find the greediest matching op name.
      String bestMatch = String.valueOf(ch);
      for (int j = i + 2; j <= arg.arg().length(); j++) {
        String nextMatch = arg.arg().substring(i, j);
        if (ALL_SPLITTABLE_OP_REPRS.contains(nextMatch)) {
          bestMatch = nextMatch;
        }
        if (!ALL_SPLITTABLE_OP_REPR_STRICT_PREFIXES.contains(nextMatch)) {
          break;
        }
      }

      // Add the op.
      splits.add(new Tokenizer.TagArg(bestMatch, startPos));
      startPos = startPos.addColumns(bestMatch.length());
      i += bestMatch.length() - 1;
    }

    String word = wordBuilder.toString();
    if (!word.isEmpty()) {
      splits.add(new Tokenizer.TagArg(word, startPos));
    }

    return splits.stream();
  }

  public static Expression parseExpression(List<Tokenizer.TagArg> args) throws CompilerException {
    Preconditions.checkArgument(!args.isEmpty());
    return parse(args, args.get(0).pos());
  }

  // Parses a full expression
  private static Expression parse(List<Tokenizer.TagArg> args, Tokenizer.Pos pos)
      throws CompilerException {
    if (args.isEmpty()) return new Tuple(ImmutableList.of(), pos);

    // Split out separators and parse atoms.
    List<Expression> atoms = new ArrayList<>();
    for (Tokenizer.TagArg arg :
        args.stream()
            .flatMap(Expression::splitSeparatorsAndOperators)
            .collect(Collectors.toList())) {
      atoms.add(parseAtom(arg.arg(), arg.pos()));
    }

    // Parse parenthesis
    ArrayDeque<Integer> stack = new ArrayDeque<>();
    for (int i = 0; i < atoms.size(); i++) {
      Expression expr = atoms.get(i);
      if (expr.type() != Type.PARENTHESIS) continue;

      Parenthesis paren = expr.cast();
      if (paren.isOpen()) {
        stack.push(i);
      } else {
        if (stack.isEmpty()) throw new CompilerException(paren.pos(), "unmatched parenthesis");

        int start = stack.pop();

        // Parse the inner range as a tuple
        Tuple tuple = Tuple.parse(atoms.subList(start + 1, i), atoms.get(start + 1).pos());
        // Replace.
        atoms.subList(start + 1, i + 1).clear();
        atoms.set(start, tuple);
        i = start;
      }
    }

    if (!stack.isEmpty())
      throw new CompilerException(atoms.get(stack.pop()).pos(), "unmatched parenthesis");

    for (Expression atom : atoms)
      if (atom.type() == Type.PARENTHESIS)
        throw new CompilerException(atom.pos(), "unexpected character");

    // Now we can do resolution.
    return parseNoSeparators(atoms);
  }

  private static void parseBinaryOperators(
      ImmutableSet<BinaryOperator> ops,
      List<Expression> atoms,
      Predicate<Expression> lhsFilter,
      Predicate<Expression> rhsFilter)
      throws CompilerException {
    for (int i = 0; i < atoms.size(); i++) {
      Expression expr = atoms.get(i);
      if (expr.type() != Type.BINARY_OPERATOR) {
        continue;
      }

      BinaryOperatorAtom binary = (BinaryOperatorAtom) expr;
      if (!ops.contains(binary.op())) continue;

      // Consume the previous and subsequent arguments.
      if (i - 1 < 0 || i + 1 >= atoms.size()) {
        throw new CompilerException(
            binary.pos(), "binary operator is missing left or right arguments");
      }

      if (!lhsFilter.test(atoms.get(i - 1)) || !rhsFilter.test(atoms.get(i + 1))) {
        continue;
      }

      // Removal of 'i - 1' shifts 'i + 1' to 'i'
      atoms.set(i - 1, new Binary(atoms.remove(i - 1), binary, atoms.remove(i)));
      i--;
    }
  }

  private static void parseBinaryOperators(ImmutableSet<BinaryOperator> ops, List<Expression> atoms)
      throws CompilerException {
    parseBinaryOperators(ops, atoms, e -> true, e -> true);
  }

  private static Expression parseNoSeparators(List<Expression> atoms) throws CompilerException {
    Preconditions.checkArgument(!atoms.isEmpty());

    // Pass 1: member references with atomics.
    parseBinaryOperators(
        ImmutableSet.of(BinaryOperator.MEMBER_REFERENCE),
        atoms,
        e -> e.type() != Expression.Type.TUPLE,
        e -> true);

    // Pass 2: call operations
    for (int i = atoms.size() - 1; i >= 0; i--) {
      Expression expr = atoms.get(i);
      if (expr.type() == Type.SCRIPT_REFERENCE) {
        throw new CompilerException(expr.pos(), "$script must be followed by parenthesis");
      }
      if (expr.type() != Type.TUPLE || i == 0) continue;

      Expression invokee = atoms.get(i - 1);
      if (invokee.type().isOperator()) continue;

      atoms.remove(i);
      atoms.set(i - 1, new UnresolvedCallOperation(invokee, expr.cast()).preResolve());
    }

    // Pass 3: member references on unresolved calls.
    parseBinaryOperators(ImmutableSet.of(BinaryOperator.MEMBER_REFERENCE), atoms);

    // Pass 4: prefix unary operators
    for (int i = atoms.size() - 1; i >= 0; i--) {
      Expression expr = atoms.get(i);
      if (expr.type() != Type.UNARY_OPERATOR) continue;

      UnaryOperatorAtom unary = (UnaryOperatorAtom) expr;
      if (unary.op().order() != UnaryOrder.PREFIX) continue;

      // Consume the subsequent argument.
      if (i + 1 >= atoms.size())
        throw new CompilerException(unary.pos(), "unary operator has no argument");
      atoms.set(i, new Unary(unary, atoms.remove(i + 1)));
    }

    // Pass 5: postfix unary operators
    for (int i = 0; i < atoms.size(); i++) {
      Expression expr = atoms.get(i);
      if (expr.type() != Type.UNARY_OPERATOR) continue;

      UnaryOperatorAtom unary = (UnaryOperatorAtom) expr;
      Preconditions.checkState(unary.op().order() == UnaryOrder.POSTFIX);

      // Consume the preceding argument.
      if (i - 1 < 0) throw new CompilerException(unary.pos(), "unary operator has no argument");
      atoms.set(i - 1, new Unary(unary, atoms.remove(i - 1)));
      i--;
    }

    // Pass 6: prefix negation operators
    for (int i = atoms.size() - 2; i >= 0; i--) {
      Expression expr = atoms.get(i);
      if (expr.type() != Type.BINARY_OPERATOR) continue;

      BinaryOperatorAtom atom = expr.cast();
      if (atom.op() != BinaryOperator.SUBTRACT) continue;

      if (i == 0 || atoms.get(i - 1).type().isOperator()) {
        UnaryNegation neg = new UnaryNegation(expr.pos(), atoms.remove(i + 1));
        atoms.set(i, neg.resolve());
      }
    }

    // Pass 7: binary operators
    for (ImmutableSet<BinaryOperator> ops : BinaryOperator.orderOfOperations()) {
      parseBinaryOperators(ops, atoms);
    }

    // In the end, we should be left with a single expression.
    if (atoms.size() > 1) {
      throw new CompilerException(
          atoms.get(1).pos(), "unexpected token: expected end of expression");
    }
    return atoms.get(0);
  }

  private final Type type;
  private final String raw;
  private final Tokenizer.Pos pos;

  private Expression(Type type, String raw, Tokenizer.Pos pos) {
    this.type = type;
    this.raw = raw;
    this.pos = pos;
  }

  public Type type() {
    return type;
  }

  public String raw() {
    return raw;
  }

  public Tokenizer.Pos pos() {
    return pos;
  }

  @Override
  public String toString() {
    return raw;
  }

  public ValueType constantValueType() throws CompilerException {
    throw new CompilerException(pos(), String.format("constant expected here, but was %s", type()));
  }

  public Expression constantValue(ValueType explicitCoersion, LabelRegistry labelRegistry)
      throws CompilerException {
    ValueType valueType = constantValueType();
    if (!explicitCoersion.canCoerce(valueType)) {
      throw new CompilerException(
          pos(), String.format("expected type %s here, but was %s", explicitCoersion, valueType));
    }

    return this;
  }

  // Returns the value type this expression computes, coerced to the provided
  // ValueType in the presence of unknowns.
  public abstract ValueType valueType(LabelRegistry labelRegistry) throws CompilerException;

  // Resolve ambiguities in the expression which require knowledge of custom defined
  // structs/types/etc.
  public Expression resolve(LabelRegistry labelRegistry) throws CompilerException {
    return this;
  }

  public abstract void compile(Compiler.Registry registry, ByteArrayDataOutput out)
      throws CompilerException;

  public final Compiler.Writer writer(Compiler.Registry registry) {
    return out -> compile(registry, out);
  }

  @SuppressWarnings("unchecked")
  public <T extends Expression> T cast() {
    return (T) this;
  }

  public <T extends Expression> T cast(Class<T> clazz) {
    return cast();
  }

  // A mutable expression holder, to support type resolution.
  @ASTNode
  public static class Holder<T extends Expression> extends Expression
      implements Expression_Holder_ASTNode {
    private T expression;

    public Holder(T expression) {
      super(expression.type(), expression.raw(), expression.pos());
      this.expression = expression;
    }

    public T get() {
      return expression;
    }

    @ASTChild
    @Override
    public Expression getExpr() {
      return expression;
    }

    public void set(T expression) {
      this.expression = expression;
    }

    @Override
    public ValueType valueType(LabelRegistry labelRegistry) throws CompilerException {
      return expression.valueType(labelRegistry);
    }

    @Override
    public void compile(Compiler.Registry registry, ByteArrayDataOutput out)
        throws CompilerException {
      expression.compile(registry, out);
    }
  }

  private abstract static class IntermediaryExpression extends Expression {
    protected IntermediaryExpression(Expression.Type type, String raw, Tokenizer.Pos pos) {
      super(type, raw, pos);
    }

    @Override
    public final <V> V accept(ASTVisitor<V> visitor, V value) {
      throw new UnsupportedOperationException();
    }

    @Override
    public final <V> V visitChildren(ASTVisitor<V> visitor, V value) {
      throw new UnsupportedOperationException();
    }

    @Override
    public final ValueType valueType(LabelRegistry labelRegistry) throws CompilerException {
      throw new UnsupportedOperationException();
    }

    @Override
    public final void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
      throw new UnsupportedOperationException();
    }
  }

  private static class Parenthesis extends IntermediaryExpression {
    private final boolean open;

    private Parenthesis(char ch, Tokenizer.Pos pos) {
      super(Type.PARENTHESIS, Character.toString(ch), pos);
      Preconditions.checkArgument(ch == '(' || ch == ')', "Bad char: %c", ch);

      open = ch == '(';
    }

    public static Optional<Parenthesis> parse(String s, Tokenizer.Pos pos) {
      if (s.equals("(") || s.equals(")")) {
        return Optional.of(new Parenthesis(s.charAt(0), pos));
      } else {
        return Optional.empty();
      }
    }

    public boolean isOpen() {
      return open;
    }
  }

  private static class Comma extends IntermediaryExpression {
    private Comma(Tokenizer.Pos pos) {
      super(Type.COMMA, ",", pos);
    }

    public static Optional<Comma> parse(String atom, Tokenizer.Pos pos) {
      if (atom.equals(",")) {
        return Optional.of(new Comma(pos));
      } else {
        return Optional.empty();
      }
    }
  }

  // A script reference is a $function call without the arguments.
  // It's essentially applied as a unary operator.
  private static class ScriptReference extends IntermediaryExpression {
    private final String function;

    public static ScriptReference parse(String atom, Tokenizer.Pos pos) throws CompilerException {
      Preconditions.checkArgument(atom.startsWith("$"));
      return new ScriptReference(validateId(atom.substring(1), pos.addColumns(1)), pos);
    }

    private ScriptReference(String function, Tokenizer.Pos pos) {
      super(Type.SCRIPT_REFERENCE, String.format("$%s", function), pos);
      this.function = function;
    }

    public String function() {
      return function;
    }
  }

  @ASTNode
  public static class Tuple extends Expression implements Expression_Tuple_ASTNode {
    private final ImmutableList<Expression> elements;

    private Tuple(ImmutableList<Expression> elements, Tokenizer.Pos pos) {
      super(
          Type.TUPLE,
          elements.stream().map(Expression::raw).collect(Collectors.joining(", ", "(", ")")),
          pos);
      this.elements = elements;
    }

    @ASTChild
    @Override
    public ImmutableList<Expression> elements() {
      return elements;
    }

    @Override
    public Expression resolve(LabelRegistry labelRegistry) throws CompilerException {
      ImmutableList.Builder<Expression> builder = ImmutableList.builder();
      for (Expression element : elements) {
        builder.add(element.resolve(labelRegistry));
      }
      ImmutableList<Expression> newElements = builder.build();
      if (newElements.size() == 1) return newElements.get(0);

      return new Tuple(newElements, pos());
    }

    private Tuple resolveTuple(LabelRegistry labelRegistry) throws CompilerException {
      Expression expr = resolve(labelRegistry);
      if (expr.type() == Type.TUPLE) return expr.cast();

      return new Tuple(ImmutableList.of(expr), pos());
    }

    @Override
    public ValueType valueType(LabelRegistry labelRegistry) throws CompilerException {
      throw new CompilerException(pos(), "parenthesis not valid in this context");
    }

    @Override
    public void compile(Compiler.Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }

    private static Tuple parse(List<Expression> atoms, Tokenizer.Pos pos) throws CompilerException {
      if (atoms.isEmpty()) return new Tuple(ImmutableList.of(), pos);

      // Separate by comma
      ImmutableList.Builder<Expression> builder = ImmutableList.builder();
      List<Expression> arg = new ArrayList<>();
      for (Expression atom : atoms) {
        if (atom.type() == Type.COMMA) {
          if (arg.isEmpty()) throw new CompilerException(atom.pos(), "unexpected comma");

          builder.add(Expression.parseNoSeparators(arg));
          arg.clear();
        } else {
          arg.add(atom);
        }
      }

      if (arg.isEmpty())
        throw new CompilerException(atoms.get(atoms.size() - 1).pos(), "unexpected comma");
      else builder.add(Expression.parseNoSeparators(arg));

      return new Tuple(builder.build(), pos);
    }
  }

  private static class UnaryOperatorAtom extends Expression {
    private final UnaryOperator op;

    private UnaryOperatorAtom(UnaryOperator op, Tokenizer.Pos pos) {
      super(Type.UNARY_OPERATOR, op.repr(), pos);
      this.op = op;
    }

    public UnaryOperator op() {
      return op;
    }

    @Override
    public ValueType valueType(LabelRegistry labelRegistry) {
      throw new UnsupportedOperationException();
    }

    @Override
    public void compile(Compiler.Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }

    @Override
    public <V> V accept(ASTVisitor<V> visitor, V value) {
      throw new UnsupportedOperationException();
    }

    @Override
    public <V> V visitChildren(ASTVisitor<V> visitor, V value) {
      throw new UnsupportedOperationException();
    }
  }

  private static class BinaryOperatorAtom extends Expression {
    private final BinaryOperator op;

    private BinaryOperatorAtom(BinaryOperator op, Tokenizer.Pos pos) {
      super(Type.BINARY_OPERATOR, op.repr(), pos);
      this.op = op;
    }

    public BinaryOperator op() {
      return op;
    }

    @Override
    public ValueType valueType(LabelRegistry labelRegistry) {
      throw new UnsupportedOperationException();
    }

    @Override
    public void compile(Compiler.Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }

    @Override
    public <V> V accept(ASTVisitor<V> visitor, V value) {
      throw new UnsupportedOperationException();
    }

    @Override
    public <V> V visitChildren(ASTVisitor<V> visitor, V value) {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class BooleanConstant extends Expression
      implements Expression_BooleanConstant_ASTNode {
    private final boolean value;

    private BooleanConstant(boolean value, Tokenizer.Pos pos) {
      super(Type.BOOLEAN_CONSTANT, Boolean.toString(value), pos);
      this.value = value;
    }

    public static BooleanConstant internal(boolean value) {
      return new BooleanConstant(value, Tokenizer.Pos.internal());
    }

    public boolean value() {
      return value;
    }

    @Override
    public ValueType constantValueType() {
      return ValueType.booleanType();
    }

    @Override
    public ValueType valueType(LabelRegistry labelRegistry) {
      return ValueType.booleanType();
    }

    @Override
    public void compile(Compiler.Registry registry, ByteArrayDataOutput out) {
      Compiler.ExprOpCodes.writeBooleanLiteral(value(), out);
    }
  }

  @ASTNode
  public static class IntegerConstant extends Expression
      implements Expression_IntegerConstant_ASTNode {
    public static final IntegerConstant NEGATIVE_ONE =
        new IntegerConstant(-1, Tokenizer.Pos.internal());

    private final int value;

    private IntegerConstant(int value, Tokenizer.Pos pos) {
      super(Type.INTEGER_CONSTANT, Integer.toString(value), pos);
      this.value = value;
    }

    public static IntegerConstant internal(int value) {
      return new IntegerConstant(value, Tokenizer.Pos.internal());
    }

    public int value() {
      return value;
    }

    @Override
    public ValueType constantValueType() {
      return ValueType.integerType();
    }

    @Override
    public ValueType valueType(LabelRegistry labelRegistry) {
      return ValueType.integerType();
    }

    @Override
    public void compile(Compiler.Registry registry, ByteArrayDataOutput out) {
      Compiler.ExprOpCodes.writeIntegerLiteral(value(), out);
    }

    public static IntegerConstant parse(String in, Tokenizer.Pos pos) throws CompilerException {
      try {
        return new IntegerConstant(Integer.parseInt(in), pos);
      } catch (Exception ex) {
        throw new CompilerException(pos, "could not parse as integer");
      }
    }
  }

  @ASTNode
  public static class DoubleConstant extends Expression
      implements Expression_DoubleConstant_ASTNode {
    private final double value;

    private DoubleConstant(double value, Tokenizer.Pos pos) {
      super(Type.DOUBLE_CONSTANT, Double.toString(value), pos);
      this.value = value;
    }

    public double value() {
      return value;
    }

    @Override
    public ValueType constantValueType() {
      return ValueType.doubleType();
    }

    @Override
    public ValueType valueType(LabelRegistry labelRegistry) {
      return ValueType.doubleType();
    }

    @Override
    public void compile(Compiler.Registry registry, ByteArrayDataOutput out) {
      Compiler.ExprOpCodes.writeDoubleLiteral(value(), out);
    }

    public static DoubleConstant parse(String in, Tokenizer.Pos pos) throws CompilerException {
      try {
        return new DoubleConstant(Double.parseDouble(in), pos);
      } catch (Exception ex) {
        throw new CompilerException(pos, "could not parse as double");
      }
    }
  }

  @ASTNode
  public static class StringLiteral extends Expression implements Expression_StringLiteral_ASTNode {
    private final String literal;

    private StringLiteral(String literal, Tokenizer.Pos pos) {
      super(Type.STRING_LITERAL, String.format("'%s'", literal), pos);
      this.literal = literal;
    }

    public String literal() {
      return literal;
    }

    @Override
    public ValueType constantValueType() {
      return ValueType.stringType();
    }

    @Override
    public ValueType valueType(LabelRegistry labelRegistry) {
      return ValueType.stringType();
    }

    @Override
    public void compile(Compiler.Registry registry, ByteArrayDataOutput out) {
      Compiler.ExprOpCodes.writeStringLiteral(literal(), out);
    }

    public static StringLiteral parse(String atom, Tokenizer.Pos pos) throws CompilerException {
      // The Tokenizer parses string literals to allow bracket quoting, so we don't have to do
      // anything.
      if (atom.length() < 2
          || atom.charAt(0) != Tokenizer.QUOTE
          || atom.charAt(atom.length() - 1) != Tokenizer.QUOTE)
        throw new CompilerException(pos, "expected string literal");

      return new StringLiteral(atom.substring(1, atom.length() - 1), pos);
    }
  }

  @ASTNode
  public static class HexColorLiteral extends Expression
      implements Expression_HexColorLiteral_ASTNode {
    private HexColorLiteral(String hex, Tokenizer.Pos pos) {
      super(Type.HEX_COLOR_LITERAL, "#" + hex, pos);
    }

    public String hex() {
      return raw().substring(1);
    }

    @Override
    public ValueType constantValueType() {
      return ValueType.hexColorType();
    }

    @Override
    public ValueType valueType(LabelRegistry labelRegistry) {
      return ValueType.hexColorType();
    }

    @Override
    public void compile(Compiler.Registry registry, ByteArrayDataOutput out) {
      Compiler.ExprOpCodes.writeHexColorLiteral(Integer.parseInt(hex(), 16), out);
    }

    public static HexColorLiteral parse(String atom, Tokenizer.Pos pos) throws CompilerException {
      Preconditions.checkArgument(atom.startsWith("#"));
      if (atom.length() != 7)
        throw new CompilerException(pos, "hex color literal should have 6 hex digits");

      for (int i = 0; i < 6; i++) {
        char ch = atom.charAt(1 + i);
        if (!Character.isDigit(ch) && !(ch >= 'a' && ch <= 'f') && !(ch >= 'A' && ch <= 'F')) {
          throw new CompilerException(pos.addColumns(1 + i), "invalid hex digit");
        }
      }

      return new HexColorLiteral(atom.substring(1), pos);
    }
  }

  public abstract static class GenericId extends Expression {
    private GenericId(Type type, String name, Tokenizer.Pos pos) {
      super(type, name, pos);
    }

    public String name() {
      return raw();
    }

    @Override
    public final <V> V visitChildren(ASTVisitor<V> visitor, V value) {
      return value;
    }
  }

  @ASTNode
  public static class Variable extends GenericId implements Expression_Variable_ASTNode {
    private Variable(String name, Tokenizer.Pos pos) {
      super(Type.VARIABLE, name, pos);
    }

    public static Variable parse(String atom, Tokenizer.Pos pos) throws CompilerException {
      return new Variable(validateId(atom, pos), pos);
    }

    @Override
    public ValueType valueType(LabelRegistry labelRegistry) throws CompilerException {
      return labelRegistry.getVariableType(raw());
    }

    @Override
    public Expression constantValue(ValueType explicitCoersion, LabelRegistry labelRegistry)
        throws CompilerException {
      if (explicitCoersion.isEnum()) {
        ValueType.EnumValueType enumType = explicitCoersion.cast();
        if (!enumType.enumType().values().contains(raw())) {
          throw new CompilerException(
              pos(),
              String.format(
                  "Expected one of %s, but was '%s'", enumType.enumType().values(), raw()));
        }
        return new EnumValueLiteral(enumType.enumType(), raw(), pos());
      } else if (explicitCoersion.isBlockId()) {
        if (!labelRegistry.isBlockDefined(raw())) {
          throw new CompilerException(pos(), String.format("'%s' is not a known BLOCK_ID", raw()));
        }
        return new BlockId(raw(), pos());
      } else if (explicitCoersion.isPromptId()) {
        if (!labelRegistry.getPrompt(raw()).isPresent()) {
          throw new CompilerException(pos(), String.format("'%s' is not a known PROMPT_ID", raw()));
        }
        return new PromptId(raw(), pos());
      } else {
        return super.constantValue(explicitCoersion, labelRegistry);
      }
    }

    @Override
    public void compile(Compiler.Registry registry, ByteArrayDataOutput out) {
      Compiler.ExprOpCodes.writeVariableReference(registry.getVariableId(name()), out);
    }
  }

  @ASTNode
  public static class StructMemberReference extends Expression
      implements Expression_StructMemberReference_ASTNode {
    private final Expression operand;
    private final String fieldName;

    private StructMemberReference(Expression operand, String fieldName) {
      super(
          Type.STRUCT_MEMBER_REFERENCE,
          String.format("%s.%s", operand.raw(), fieldName),
          operand.pos());
      this.operand = operand;
      this.fieldName = fieldName;
    }

    @Override
    public ValueType valueType(LabelRegistry labelRegistry) throws CompilerException {
      ValueType operandType = operand.valueType(labelRegistry);
      if (!operandType.isStruct())
        throw new CompilerException(operand.pos(), "expected struct, but was: " + operandType);

      StructType structType = ((ValueType.StructValueType) operandType).structType();
      return structType.fieldValueType(fieldName, pos());
    }

    @Override
    public void compile(Compiler.Registry registry, ByteArrayDataOutput out)
        throws CompilerException {
      StructType structType =
          ((ValueType.StructValueType) operand.valueType(registry.labelRegistry())).structType();
      Compiler.ExprOpCodes.writeMemberReference(
          operand.writer(registry), registry.getFieldNumber(structType, fieldName), out);
    }
  }

  @ASTNode
  public static final class TypeLiteral extends Expression
      implements Expression_TypeLiteral_ASTNode {
    private final ValueType.TypeLiteralType literalType;

    private TypeLiteral(ValueType.TypeLiteralType literalType, Tokenizer.Pos pos) {
      super(Type.TYPE_LITERAL, literalType.toString(), pos);
      this.literalType = literalType;
    }

    public ValueType.TypeLiteralType literalType() {
      return literalType;
    }

    @Override
    public ValueType valueType(LabelRegistry labelRegistry) throws CompilerException {
      return literalType;
    }

    @Override
    public void compile(Compiler.Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static final class TypeAnnotatedExpression extends Expression
      implements Expression_TypeAnnotatedExpression_ASTNode {
    private final Expression expression;
    private final TypeLiteral annotation;

    private TypeAnnotatedExpression(Expression expression, TypeLiteral annotation) {
      super(
          Type.TYPE_ANNOTATED_EXPRESSION,
          String.format("%s:%s", expression.raw(), annotation.toString()),
          expression.pos());
      this.expression = expression;
      this.annotation = annotation;
    }

    @ASTChild
    @Override
    public Expression expression() {
      return expression;
    }

    @ASTChild
    @Override
    public TypeLiteral annotation() {
      return annotation;
    }

    @Override
    public ValueType valueType(LabelRegistry labelRegistry) throws CompilerException {
      return annotation.valueType(labelRegistry);
    }

    @Override
    public Expression resolve(LabelRegistry labelRegistry) throws CompilerException {
      throw new UnsupportedOperationException();
    }

    @Override
    public void compile(Compiler.Registry registry, ByteArrayDataOutput out) {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class BlockId extends GenericId implements Expression_BlockId_ASTNode {
    private BlockId(String name, Tokenizer.Pos pos) {
      super(Type.BLOCK_ID, name, pos);
    }

    public static BlockId parse(String atom, Tokenizer.Pos pos) throws CompilerException {
      return new BlockId(validateId(atom, pos), pos);
    }

    @Override
    public ValueType valueType(LabelRegistry labelRegistry) {
      return ValueType.BLOCK_ID_TYPE;
    }

    @Override
    public ValueType constantValueType() {
      return ValueType.BLOCK_ID_TYPE;
    }

    @Override
    public void compile(Compiler.Registry registry, ByteArrayDataOutput out) {
      Compiler.ExprOpCodes.writeIntegerLiteral(registry.getBlockId(this), out);
    }
  }

  @ASTNode
  public static class SubBlockId extends GenericId implements Expression_SubBlockId_ASTNode {
    private SubBlockId(String name, Tokenizer.Pos pos) {
      super(Type.SUB_BLOCK_ID, name, pos);
    }

    public static SubBlockId parse(String atom, Tokenizer.Pos pos) throws CompilerException {
      return new SubBlockId(validateId(atom, pos), pos);
    }

    @Override
    public ValueType valueType(LabelRegistry labelRegistry) {
      return ValueType.SUB_BLOCK_ID_TYPE;
    }

    @Override
    public ValueType constantValueType() {
      return ValueType.SUB_BLOCK_ID_TYPE;
    }

    @Override
    public void compile(Compiler.Registry registry, ByteArrayDataOutput out) {
      Compiler.ExprOpCodes.writeIntegerLiteral(registry.getSubBlockId(this), out);
    }
  }

  @ASTNode
  public static class PromptId extends GenericId implements Expression_PromptId_ASTNode {
    private PromptId(String name, Tokenizer.Pos pos) {
      super(Type.PROMPT_ID, name, pos);
    }

    public static PromptId parse(String atom, Tokenizer.Pos pos) throws CompilerException {
      return new PromptId(validateId(atom, pos), pos);
    }

    @Override
    public ValueType valueType(LabelRegistry labelRegistry) {
      return ValueType.PROMPT_ID_TYPE;
    }

    @Override
    public ValueType constantValueType() {
      return ValueType.PROMPT_ID_TYPE;
    }

    @Override
    public void compile(Compiler.Registry registry, ByteArrayDataOutput out) {
      Compiler.ExprOpCodes.writeIntegerLiteral(registry.getPromptId(name()), out);
    }
  }

  @ASTNode
  public static class EnumValueLiteral extends Expression
      implements Expression_EnumValueLiteral_ASTNode {
    private final TypedTag.DefineEnum enumType;
    private final String value;

    public EnumValueLiteral(TypedTag.DefineEnum enumType, String value, Tokenizer.Pos pos) {
      super(Type.ENUM_VALUE_LITERAL, String.format("%s.%s", enumType.typeName(), value), pos);
      Preconditions.checkArgument(enumType.hasValue(value));

      this.enumType = enumType;
      this.value = value;
    }

    public TypedTag.DefineEnum enumType() {
      return enumType;
    }

    public String value() {
      return value;
    }

    public int intValue() {
      return enumType.values().asList().indexOf(value);
    }

    @Override
    public ValueType constantValueType() {
      return enumType.valueType();
    }

    @Override
    public ValueType valueType(LabelRegistry labelRegistry) throws CompilerException {
      return enumType.valueType();
    }

    @Override
    public void compile(Compiler.Registry registry, ByteArrayDataOutput out) {
      Compiler.ExprOpCodes.writeIntegerLiteral(intValue(), out);
    }
  }

  @ASTNode
  public static class UnresolvedCallOperation extends Expression
      implements Expression_UnresolvedCallOperation_ASTNode {
    private final Expression invokee;
    private final Tuple args;

    private UnresolvedCallOperation(Expression invokee, Tuple args) {
      super(
          Type.UNRESOLVED_CALL_OPERATION,
          String.format("%s(%s)", invokee.raw(), args.raw()),
          invokee.pos());
      this.invokee = invokee;
      this.args = args;
    }

    @ASTChild
    @Override
    public Expression invokee() {
      return invokee;
    }

    @ASTChild
    @Override
    public Tuple args() {
      return args;
    }

    private Expression preResolve() throws CompilerException {
      switch (invokee.type()) {
        case SCRIPT_REFERENCE:
          return new Script(invokee.cast(), args);
        default:
          return this;
      }
    }

    @Override
    public Expression resolve(LabelRegistry labelRegistry) throws CompilerException {
      Expression resolved = invokee.resolve(labelRegistry);
      switch (resolved.type()) {
        case SCRIPT_REFERENCE:
          {
            return new Script(resolved.cast(), args).resolve(labelRegistry);
          }
        case TYPE_LITERAL:
          {
            TypeLiteral typeLiteral = resolved.cast();
            if (typeLiteral.literalType().valueType().isStruct()) {
              return new StructInstance(typeLiteral, args, labelRegistry);
            }

            break;
          }
        default:
          break;
      }

      throw new CompilerException(
          pos(),
          String.format(
              "Expected $script or Struct.Type invokee for operator(), but was %s",
              invokee.type()));
    }

    @Override
    public ValueType valueType(LabelRegistry labelRegistry) throws CompilerException {
      throw new UnsupportedOperationException();
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
      throw new UnsupportedOperationException();
    }
  }

  @ASTNode
  public static class StructInstance extends Expression
      implements Expression_StructInstance_ASTNode {

    @ASTNode
    @AutoValue
    public abstract static class FieldInitializer
        implements Expression_StructInstance_FieldInitializer_ASTNode {

      public abstract Variable fieldNameVariable();

      public String name() {
        return fieldNameVariable().raw();
      }

      public Tokenizer.Pos pos() {
        return fieldNameVariable().pos();
      }

      @ASTChild
      @Override
      public abstract Expression value();

      private static FieldInitializer parse(Expression kwarg, LabelRegistry labelRegistry)
          throws CompilerException {
        if (kwarg.type() != Type.BINARY) {
          throw new CompilerException(kwarg.pos(), "Expected struct field assignment here");
        }
        Binary binary = kwarg.cast();
        if (binary.op() != BinaryOperator.ASSIGNMENT) {
          throw new CompilerException(kwarg.pos(), "Expected struct field assignment here");
        }
        if (binary.lhs().type() != Type.VARIABLE) {
          throw new CompilerException(binary.lhs().pos(), "Expected struct field name");
        }

        return new AutoValue_Expression_StructInstance_FieldInitializer(
            binary.lhs().cast(), binary.rhs().resolve(labelRegistry));
      }
    }

    private final TypeLiteral typeLiteral;
    private final StructType structType;
    private final ImmutableMap<String, FieldInitializer> kwargs;

    private StructInstance(TypeLiteral typeLiteral, Tuple args, LabelRegistry labelRegistry)
        throws CompilerException {
      super(
          Type.STRUCT_INSTANCE,
          String.format("%s(%s)", typeLiteral.raw(), args.raw()),
          typeLiteral.pos());

      this.typeLiteral = typeLiteral;

      ValueType.StructValueType structValueType = typeLiteral.literalType().valueType().cast();
      this.structType = structValueType.structType();

      Set<String> required = new HashSet<>(structType().requiredFieldNames());
      Map<String, FieldInitializer> kwargs = new HashMap<>();
      for (Expression expr : args.elements()) {
        FieldInitializer field = FieldInitializer.parse(expr, labelRegistry);
        required.remove(field.name());

        Optional<StructType.Field> fieldDef = structType().field(field.name());
        if (!fieldDef.isPresent()) {
          throw new CompilerException(
              field.pos(),
              String.format(
                  "field '%s' is undefined for the type 'Struct.%s'",
                  field.name(), structType().typeName()));
        }

        ValueType actual = field.value().valueType(labelRegistry);
        ValueType expected = fieldDef.get().valueType();
        if (!expected.canCoerce(actual)) {
          throw new CompilerException(
              field.value().pos(),
              String.format(
                  "expected type %s for field %s, but got type %s",
                  expected, field.name(), actual));
        }

        if (kwargs.putIfAbsent(field.name(), field) != null) {
          throw new CompilerException(
              field.pos(), String.format("duplicate kwarg: '%s'", field.name()));
        }
      }
      if (!required.isEmpty()) {
        throw new CompilerException(
            typeLiteral.pos(),
            String.format(
                "struct initializer is missing required fields: '%s'",
                required.stream().sorted().collect(Collectors.joining("', '"))));
      }

      this.kwargs = ImmutableMap.copyOf(kwargs);
    }

    @ASTChild
    @Override
    public TypeLiteral structTypeLiteral() {
      return typeLiteral;
    }

    public StructType structType() {
      return structType;
    }

    @ASTChild
    @Override
    public ImmutableCollection<FieldInitializer> fieldInitializers() {
      return kwargs.values();
    }

    @Override
    public ValueType valueType(LabelRegistry labelRegistry) throws CompilerException {
      return typeLiteral.literalType().valueType();
    }

    @Override
    public ValueType constantValueType() throws CompilerException {
      // Check that all the field initializers are constants.
      for (FieldInitializer field : fieldInitializers()) {
        field.value().constantValueType();
      }

      return typeLiteral.literalType().valueType();
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
      Map<Integer, Compiler.Writer> fieldWriters =
          fieldInitializers()
              .stream()
              .collect(
                  ImmutableMap.toImmutableMap(
                      f -> registry.getFieldNumber(structType, f.name()),
                      f -> f.value().writer(registry)));
      Compiler.ExprOpCodes.writeStructInitializer(
          registry.getStructId(structType), fieldWriters, out);
    }
  }

  @ASTNode
  public static class Script extends Expression implements Expression_Script_ASTNode {
    private final ScriptReference scriptReference;
    private final Tuple args;

    private Script(ScriptReference scriptReference, Tuple args) {
      super(
          Type.SCRIPT,
          String.format("%s(%s)", scriptReference.raw(), args.raw()),
          scriptReference.pos());
      this.scriptReference = scriptReference;
      this.args = args;
    }

    public String function() {
      return scriptReference.function();
    }

    public int numArgs() {
      return args.elements().size();
    }

    public Expression arg(int index) {
      return args.elements().get(index);
    }

    @ASTChild
    @Override
    public ImmutableList<Expression> args() {
      return args.elements();
    }

    @Override
    public Script resolve(LabelRegistry labelRegistry) throws CompilerException {
      if (!labelRegistry.scriptDefinition(function()).isPresent()) {
        throw new CompilerException(
            pos(), String.format("unrecognized function '$%s'", function()));
      }

      return new Script(scriptReference, args.resolveTuple(labelRegistry));
    }

    @Override
    public void compile(Compiler.Registry registry, ByteArrayDataOutput out)
        throws CompilerException {
      Compiler.ExprOpCodes.writeScriptInvocation(
          registry.getScriptId(scriptReference.function()),
          args.elements()
              .stream()
              .map(e -> e.writer(registry))
              .collect(ImmutableList.toImmutableList()),
          out);
    }

    @Override
    public ValueType valueType(LabelRegistry labelRegistry) throws CompilerException {
      TypedTag.ExternScript scriptDef = labelRegistry.scriptDefinition(function()).get();

      // Validate arguments.
      if (scriptDef.numArgs() != args.elements().size())
        throw new CompilerException(
            pos(),
            String.format(
                "script expected %d arguments, but found %d",
                scriptDef.numArgs(), args.elements().size()));

      for (int i = 0; i < scriptDef.numArgs(); i++) {
        ValueType actual = args.elements().get(i).valueType(labelRegistry);
        ValueType expected = scriptDef.argType(i).literalType().valueType();

        if (!actual.canCoerce(expected))
          throw new CompilerException(
              args.elements().get(i).pos(),
              String.format("expected type %s in this position, but was %s", expected, actual));
      }

      return scriptDef.returnType().literalType().valueType();
    }
  }

  @ASTNode
  public static class Unary extends Expression implements Expression_Unary_ASTNode {
    private final UnaryOperatorAtom op;
    private final Expression arg;

    private static String repr(UnaryOperator op, Expression arg) {
      switch (op.order()) {
        case PREFIX:
          return op.repr() + " " + arg.raw();
        case POSTFIX:
          return arg.raw() + " " + op.repr();
        default:
          throw new AssertionError(op.order());
      }
    }

    private Unary(UnaryOperatorAtom op, Expression arg) {
      super(Type.UNARY, repr(op.op(), arg), op.pos());
      this.op = op;
      this.arg = arg;
    }

    public UnaryOperator op() {
      return op.op();
    }

    public Tokenizer.Pos opPos() {
      return op.pos();
    }

    @ASTChild
    @Override
    public Expression arg() {
      return arg;
    }

    @Override
    public ValueType valueType(LabelRegistry labelRegistry) throws CompilerException {
      switch (op.op()) {
        case NOT:
          {
            ValueType inner = arg.valueType(labelRegistry);
            if (!inner.isBoolean())
              throw new CompilerException(
                  opPos(), "operator 'not' requires BOOLEAN, but was " + inner);
            return ValueType.booleanType();
          }
        case INCREMENT:
        case DECREMENT:
          {
            String raw = op.op().repr();
            if (!arg.type().isAssignable())
              throw new CompilerException(
                  opPos(), String.format("operator '%s' requires a stand-alone variable", raw));

            ValueType inner = arg.valueType(labelRegistry);
            if (inner.isNumeric()) return inner;
            else
              throw new CompilerException(
                  opPos(),
                  String.format("operator '%s' requires a numeric type, but was %s", raw, inner));
          }
        default:
          throw new AssertionError(op.op());
      }
    }

    @Override
    public Expression resolve(LabelRegistry labelRegistry) throws CompilerException {
      return new Unary(op, arg.resolve(labelRegistry));
    }

    @Override
    public void compile(Compiler.Registry registry, ByteArrayDataOutput out)
        throws CompilerException {
      switch (op.op()) {
        case NOT:
          Compiler.ExprOpCodes.writeBooleanNegation(arg.writer(registry), out);
          break;
        case INCREMENT:
          Compiler.ExprOpCodes.writePostIncrement(arg.writer(registry), out);
          break;
        case DECREMENT:
          Compiler.ExprOpCodes.writePostDecrement(arg.writer(registry), out);
          break;
        default:
          throw new AssertionError(op.op());
      }
    }
  }

  @ASTNode
  public static class UnaryNegation extends Expression implements Expression_UnaryNegation_ASTNode {
    private final Expression expr;

    private UnaryNegation(Tokenizer.Pos negationPos, Expression expr) {
      super(Type.UNARY_NEGATION, "-" + expr.raw(), negationPos);
      this.expr = expr;
    }

    @ASTChild
    @Override
    public Expression expr() {
      return expr;
    }

    public Expression resolve() {
      switch (expr.type()) {
        case INTEGER_CONSTANT:
          {
            IntegerConstant x = expr.cast();
            return new IntegerConstant(-x.value(), pos());
          }
        case DOUBLE_CONSTANT:
          {
            DoubleConstant x = expr.cast();
            return new DoubleConstant(-x.value(), pos());
          }
        default:
          return this;
      }
    }

    @Override
    public Expression resolve(LabelRegistry labelRegistry) throws CompilerException {
      return new UnaryNegation(pos(), expr().resolve(labelRegistry));
    }

    @Override
    public ValueType valueType(LabelRegistry labelRegistry) throws CompilerException {
      ValueType exprType = expr.valueType(labelRegistry);
      if (exprType.isNumeric()) {
        return exprType;
      } else {
        throw new CompilerException(
            expr.pos(), String.format("Expected numeric type here, but was %s", exprType));
      }
    }

    @Override
    public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
      Compiler.ExprOpCodes.writeMultiplication(
          IntegerConstant.NEGATIVE_ONE.writer(registry), expr.writer(registry), out);
    }
  }

  @ASTNode
  public static class Binary extends Expression implements Expression_Binary_ASTNode {
    private final Expression lhs;
    private final BinaryOperatorAtom op;
    private final Expression rhs;

    private Binary(Expression lhs, BinaryOperatorAtom op, Expression rhs) {
      super(Type.BINARY, lhs.raw() + " " + op.raw() + " " + rhs.raw(), op.pos());
      this.lhs = lhs;
      this.op = op;
      this.rhs = rhs;
    }

    @ASTChild
    @Override
    public Expression lhs() {
      return lhs;
    }

    public BinaryOperator op() {
      return op.op();
    }

    public Tokenizer.Pos opPos() {
      return op.pos();
    }

    @ASTChild
    @Override
    public Expression rhs() {
      return rhs;
    }

    @Override
    public ValueType valueType(LabelRegistry labelRegistry) throws CompilerException {
      String opRaw = op().repr();
      switch (op()) {
        case ADD:
        case SUBTRACT:
        case MULTIPLY:
        case DIVIDE:
          {
            ValueType lValue = lhs.valueType(labelRegistry);
            ValueType rValue = rhs.valueType(labelRegistry);

            if (!lValue.isNumeric() || !rValue.isNumeric()) throw undefined(lValue, rValue);

            if (op() == BinaryOperator.DIVIDE
                && rValue.isInteger()
                && rhs.type() == Type.INTEGER_CONSTANT) {
              IntegerConstant constant = rhs.cast();
              if (constant.value() == 0)
                throw new CompilerException(opPos(), "Yes, I actually coded a check for div/0");
            }

            if (lValue.isDouble() || rValue.isDouble()) return ValueType.doubleType();
            else return ValueType.integerType();
          }
        case AND:
        case OR:
          {
            ValueType lValue = lhs.valueType(labelRegistry);
            ValueType rValue = rhs.valueType(labelRegistry);

            if (!lValue.isBoolean() || !rValue.isBoolean()) throw undefined(lValue, rValue);
            return ValueType.booleanType();
          }
        case GREATER_THAN:
        case GREATER_THAN_OR_EQUAL:
        case LESS_THAN:
        case LESS_THAN_OR_EQUAL:
          {
            ValueType lValue = lhs.valueType(labelRegistry);
            ValueType rValue = rhs.valueType(labelRegistry);

            if (lValue.isNumeric() && rValue.isNumeric()) return ValueType.booleanType();
            else if (lValue.supportsComparison()
                && rValue.supportsComparison()
                && lValue.equals(rValue)) return ValueType.booleanType();
            else throw undefined(lValue, rValue);
          }
        case EQUAL:
        case NOT_EQUAL:
          {
            ValueType lValue = lhs.valueType(labelRegistry);
            ValueType rValue = rhs.valueType(labelRegistry);

            if (lValue.isDouble() || rValue.isDouble())
              throw new CompilerException(
                  opPos(),
                  String.format(
                      "operator '%s' is unsafe with DOUBLE arguments.  Use integers, or perform different comparisons",
                      opRaw));
            if (!lValue.supportsEquals() || !rValue.supportsEquals() || !lValue.equals(rValue))
              throw undefined(lValue, rValue);
            else return ValueType.booleanType();
          }
        case ASSIGNMENT:
          {
            if (!lhs.type().isAssignable())
              throw new CompilerException(
                  opPos(), "operator '=' requires a variable on the left hand side");
            ValueType lValue = lhs.valueType(labelRegistry);
            ValueType rValue = rhs.valueType(labelRegistry);

            if (lValue.isStruct())
              throw new CompilerException(opPos(), "Structs cannot be reassigned");
            if (!lValue.equals(rValue) && (!lValue.isDouble() || !rValue.isInteger()))
              throw new CompilerException(
                  opPos(),
                  String.format(
                      "attempting to assign %s to '%s', which has type %s",
                      rValue, lhs.raw(), lValue));
            return lValue;
          }
        case MINUS_EQUALS:
        case PLUS_EQUALS:
          {
            if (!lhs.type().isAssignable())
              throw new CompilerException(
                  opPos(),
                  String.format("operator '%s' requires a variable on the left hand side", opRaw));

            ValueType lValue = lhs.valueType(labelRegistry);
            ValueType rValue = rhs.valueType(labelRegistry);

            if (!rValue.isNumeric()) throw undefined(lValue, rValue);
            if (lValue.isInteger() && rValue.isDouble())
              throw new CompilerException(
                  opPos(),
                  String.format(
                      "attempting to redefine variable '%s' of type INTEGER to type DOUBLE",
                      lhs.raw()));

            return lValue;
          }
        default:
          throw new AssertionError(op());
      }
    }

    @Override
    public Expression constantValue(ValueType explicitCoersion, LabelRegistry labelRegistry)
        throws CompilerException {
      // Allow decimals in double constants.
      if (explicitCoersion.isDouble()
          && op() == BinaryOperator.MEMBER_REFERENCE
          && lhs.type() == Type.INTEGER_CONSTANT
          && rhs.type() == Type.INTEGER_CONSTANT) {
        return DoubleConstant.parse(String.format("%s.%s", lhs.raw(), rhs.raw()), lhs.pos());
      }

      return super.constantValue(explicitCoersion, labelRegistry);
    }

    @Override
    public Expression resolve(LabelRegistry labelRegistry) throws CompilerException {
      if (op() == BinaryOperator.MEMBER_REFERENCE) {
        // Allow decimals in double constants.
        if (lhs.type() == Type.INTEGER_CONSTANT && rhs.type() == Type.INTEGER_CONSTANT)
          return DoubleConstant.parse(String.format("%s.%s", lhs.raw(), rhs.raw()), lhs.pos());

        // rhs must be a raw variable.
        if (rhs.type() != Type.VARIABLE)
          throw new CompilerException(this.rhs.pos(), "unexpected expression after '.' operator");
        Variable rhs = this.rhs.cast();

        if (lhs.type() == Type.TYPE_LITERAL) {
          TypeLiteral type = lhs.cast();
          ValueType valueType = type.literalType().valueType();
          if (valueType.equals(ValueType.BLOCK_ID_TYPE)) {
            if (!labelRegistry.isBlockDefined(rhs.name()))
              throw new CompilerException(
                  rhs.pos(), String.format("Block id '%s' does not exist", rhs.name()));

            return new BlockId(rhs.name(), rhs.pos());
          } else if (valueType.equals(ValueType.PROMPT_ID_TYPE)) {
            if (!labelRegistry.getPrompt(rhs.name()).isPresent())
              throw new CompilerException(
                  rhs.pos(), String.format("Prompt id '%s' does not exist", rhs.name()));

            return new PromptId(rhs.name(), rhs.pos());
          } else {
            throw new CompilerException(
                lhs.pos(), String.format("Type literal '%s' is not a namespace", lhs.raw()));
          }
        } else if (lhs.type() == Type.VARIABLE) {
          Variable var = lhs.cast();

          // Check for ENUM and STRUCT type literals.
          if (var.name().equals("ENUM")) {
            Optional<TypedTag.DefineEnum> enumType = labelRegistry.getEnumType(rhs.name());
            if (!enumType.isPresent())
              throw new CompilerException(
                  rhs.pos(), String.format("ENUM.%s does not exist", rhs.name()));

            return new TypeLiteral(enumType.get().valueType().asTypeLiteral(), var.pos());
          } else if (var.name().equals("STRUCT")) {
            Optional<StructType> structType = labelRegistry.getStructType(rhs.name());
            if (!structType.isPresent())
              throw new CompilerException(
                  rhs.pos(), String.format("STRUCT.%s does not exist", rhs.name()));

            return new TypeLiteral(structType.get().valueType().asTypeLiteral(), var.pos());
          } else {
            // Check if it's an Enum constant, which must be resolved to a constant before full
            // value type resolution.
            Optional<TypedTag.DefineEnum> enumType = labelRegistry.getEnumType(var.name());
            if (enumType.isPresent()) {
              if (!enumType.get().hasValue(rhs.name()))
                throw new CompilerException(
                    rhs.pos(),
                    String.format(
                        "enum type '%s' has no value '%s'", enumType.get().typeName(), rhs.name()));

              return new EnumValueLiteral(enumType.get(), rhs.name(), lhs.pos());
            }
          }
        }

        // Otherwise, it's a struct reference.
        return new StructMemberReference(lhs.resolve(labelRegistry), rhs.name());
      } else if (op() == BinaryOperator.TYPE_ANNOTATOR) {
        Expression rhs = this.rhs.resolve(labelRegistry);
        if (rhs.type() != Type.TYPE_LITERAL)
          throw new CompilerException(rhs.pos(), "expected TYPE_LITERAL here");
        TypeLiteral typeLiteral = rhs.cast();

        // LHS must be a variable or a script.
        Expression lhs = this.lhs.resolve(labelRegistry);
        if (lhs.type() != Type.VARIABLE && lhs.type() != Type.SCRIPT) {
          throw new CompilerException(
              lhs.pos(),
              String.format(
                  "expected VARIABLE or SCRIPT here before type annotator, but was %s",
                  lhs.type()));
        }

        if (typeLiteral.literalType().valueType().isVoid() && lhs.type() != Type.SCRIPT) {
          throw new CompilerException(
              rhs.pos(), "VOID type is only applicable to $scripts, not parameters");
        }

        return new TypeAnnotatedExpression(lhs, typeLiteral);
      } else {
        // It's irreducible
        return new Binary(lhs.resolve(labelRegistry), op, rhs.resolve(labelRegistry));
      }
    }

    @Override
    public void compile(Compiler.Registry registry, ByteArrayDataOutput out)
        throws CompilerException {
      switch (op.op()) {
        case ADD:
          Compiler.ExprOpCodes.writeAddition(lhs.writer(registry), rhs.writer(registry), out);
          break;
        case AND:
          Compiler.ExprOpCodes.writeLogicalAnd(lhs.writer(registry), rhs.writer(registry), out);
          break;
        case ASSIGNMENT:
          Compiler.ExprOpCodes.writeAssignment(lhs.writer(registry), rhs.writer(registry), out);
          break;
        case DIVIDE:
          Compiler.ExprOpCodes.writeDivision(lhs.writer(registry), rhs.writer(registry), out);
          break;
        case EQUAL:
          Compiler.ExprOpCodes.writeEquals(lhs.writer(registry), rhs.writer(registry), out);
          break;
        case GREATER_THAN:
          // Flip the arguments
          Compiler.ExprOpCodes.writeLessThan(rhs.writer(registry), lhs.writer(registry), out);
          break;
        case GREATER_THAN_OR_EQUAL:
          // Negate lessThan
          Compiler.ExprOpCodes.writeBooleanNegation(
              o ->
                  Compiler.ExprOpCodes.writeLessThan(lhs.writer(registry), rhs.writer(registry), o),
              out);
          break;
        case LESS_THAN:
          Compiler.ExprOpCodes.writeLessThan(lhs.writer(registry), rhs.writer(registry), out);
          break;
        case LESS_THAN_OR_EQUAL:
          // Negate greaterThan - flip the arguments
          Compiler.ExprOpCodes.writeBooleanNegation(
              o ->
                  Compiler.ExprOpCodes.writeLessThan(rhs.writer(registry), lhs.writer(registry), o),
              out);
          break;
        case MINUS_EQUALS:
          // Set equal to itself.
          Compiler.ExprOpCodes.writeAssignment(
              lhs.writer(registry),
              o ->
                  Compiler.ExprOpCodes.writeSubtraction(
                      lhs.writer(registry), rhs.writer(registry), o),
              out);
          break;
        case MULTIPLY:
          Compiler.ExprOpCodes.writeMultiplication(lhs.writer(registry), rhs.writer(registry), out);
          break;
        case NOT_EQUAL:
          Compiler.ExprOpCodes.writeBooleanNegation(
              o -> Compiler.ExprOpCodes.writeEquals(lhs.writer(registry), rhs.writer(registry), o),
              out);
          break;
        case OR:
          Compiler.ExprOpCodes.writeLogicalOr(lhs.writer(registry), rhs.writer(registry), out);
          break;
        case PLUS_EQUALS:
          // Set equal to itself.
          Compiler.ExprOpCodes.writeAssignment(
              lhs.writer(registry),
              o ->
                  Compiler.ExprOpCodes.writeAddition(lhs.writer(registry), rhs.writer(registry), o),
              out);
          break;
        case SUBTRACT:
          Compiler.ExprOpCodes.writeSubtraction(lhs.writer(registry), rhs.writer(registry), out);
          break;
        default:
          throw new AssertionError(op.op());
      }
    }

    private CompilerException undefined(ValueType lValue, ValueType rValue) {
      return new CompilerException(
          opPos(),
          String.format("operator '%s %s %s' is not defined", lValue, op().repr(), rValue));
    }
  }
}
