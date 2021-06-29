package tgs;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import com.google.auto.value.AutoValue;
import com.google.auto.value.extension.memoized.Memoized;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.errorprone.annotations.ForOverride;

@AutoValue
public abstract class StructType {

  @AutoValue
  public abstract static class Field {
    public abstract Tokenizer.TagArg nameArg();

    public final String name() {
      return nameArg().arg();
    }

    public final Tokenizer.Pos namePos() {
      return nameArg().pos();
    }

    public abstract Expression.ValueType valueType();

    public abstract Optional<Expression> defaultValue();

    public static Field create(Tokenizer.TagArg name, Expression value) throws CompilerException {
      return new AutoValue_StructType_Field(name, value.constantValueType(), Optional.of(value));
    }

    public static Field create(Tokenizer.TagArg name, Expression.ValueType valueType) {
      return new AutoValue_StructType_Field(name, valueType, Optional.empty());
    }

    private static Field createInternal(String name, Expression value) {
      try {
        return create(new Tokenizer.TagArg(name, Tokenizer.Pos.internal()), value);
      } catch (CompilerException ex) {
        throw new AssertionError(ex);
      }
    }
  }

  private static final StructType KEY =
      StructType.builder("Key")
          .addFieldInternal(
              Field.createInternal("obtained", Expression.BooleanConstant.internal(false)))
          .addFieldInternal(Field.createInternal("count", Expression.IntegerConstant.internal(0)))
          .build();

  public static StructType key() {
    return KEY;
  }

  public abstract String typeName();

  public abstract ImmutableMap<String, Field> fields();

  @Memoized
  public ImmutableSet<String> requiredFieldNames() {
    return fields()
        .values()
        .stream()
        .filter(f -> !f.defaultValue().isPresent())
        .map(f -> f.name())
        .collect(ImmutableSet.toImmutableSet());
  }

  public Optional<Field> field(String name) {
    return Optional.ofNullable(fields().get(name));
  }

  public final Expression.ValueType fieldValueType(String fieldName, Tokenizer.Pos pos)
      throws CompilerException {
    Field field = fields().get(fieldName);
    if (field == null)
      throw new CompilerException(
          pos, String.format("Struct '%s' does not have a field named '%s'", fieldName));

    return field.valueType();
  }

  @Memoized
  public Expression.ValueType.StructValueType valueType() {
    return Expression.ValueType.StructValueType.of(this);
  }

  public static Builder builder(String typeName) {
    return new AutoValue_StructType.Builder().setTypeName(typeName);
  }

  @AutoValue.Builder
  public abstract static class Builder {

    private Map<String, Field> fieldsBuilder = new HashMap<>();

    public abstract Builder setTypeName(String typeName);

    @ForOverride
    abstract Builder setFields(Map<String, Field> fields);

    public Builder addField(Field field) throws CompilerException {
      if (fieldsBuilder.containsKey(field.name()))
        throw new CompilerException(field.namePos(), "duplicate field for struct");

      fieldsBuilder.put(field.name(), field);
      return this;
    }

    private Builder addFieldInternal(Field field) {
      try {
        return addField(field);
      } catch (CompilerException ex) {
        throw new AssertionError(ex);
      }
    }

    @ForOverride
    abstract StructType autoBuild();

    public final StructType build() {
      return setFields(fieldsBuilder).autoBuild();
    }
  }
}
