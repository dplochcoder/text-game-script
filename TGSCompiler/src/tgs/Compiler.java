package tgs;

import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import com.google.auto.value.AutoValue;
import com.google.common.base.Verify;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableTable;
import com.google.common.io.ByteArrayDataOutput;
import com.google.common.io.ByteStreams;

// See the various 'OpCodes' classes for the bytecode specification.
public class Compiler {

  public interface Writer {
    void write(ByteArrayDataOutput out) throws CompilerException;
  }

  // Op Codes
  public static class ExprOpCodes {
    private ExprOpCodes() {}

    private static final int VARIABLE_REFERENCE = 1; // varint id

    public static void writeVariableReference(int variableId, ByteArrayDataOutput out) {
      writeVarint(VARIABLE_REFERENCE, out);
      writeVarint(variableId, out);
    }

    private static final int MEMBER_REFERENCE = 2; // Expression, varint field number

    public static void writeMemberReference(
        Writer operandWriter, int fieldNumber, ByteArrayDataOutput out) throws CompilerException {
      writeVarint(MEMBER_REFERENCE, out);
      operandWriter.write(out);
      writeVarint(fieldNumber, out);
    }

    private static final int SCRIPT_INVOCATION = 3; // varint id, N args

    public static void writeScriptInvocation(
        int scriptId, Iterable<Writer> argumentWriters, ByteArrayDataOutput out)
        throws CompilerException {
      writeVarint(SCRIPT_INVOCATION, out);
      writeVarint(scriptId, out);
      for (Writer writer : argumentWriters) {
        writer.write(out);
      }
    }

    private static final int BOOLEAN_LITERAL = 10; // 0 = false, 1 = true

    public static void writeBooleanLiteral(boolean value, ByteArrayDataOutput out) {
      writeVarint(BOOLEAN_LITERAL, out);
      out.writeByte(value ? 1 : 0);
    }

    private static final int INTEGER_LITERAL = 11; // 32-bit signed

    public static void writeIntegerLiteral(int value, ByteArrayDataOutput out) {
      writeVarint(INTEGER_LITERAL, out);
      out.writeInt(value);
    }

    private static final int DOUBLE_LITERAL = 12; // String decimal

    public static void writeDoubleLiteral(double value, ByteArrayDataOutput out) {
      writeVarint(DOUBLE_LITERAL, out);
      writeUTF8(Double.toString(value), out);
    }

    private static final int STRING_LITERAL = 13; // Varint size N, UTF 8 bytes

    public static void writeStringLiteral(String value, ByteArrayDataOutput out) {
      writeVarint(STRING_LITERAL, out);
      writeUTF8(value, out);
    }

    private static final int HEX_COLOR_LITERAL = 14; // 4 bytes ARGB

    public static void writeHexColorLiteral(int rgb, ByteArrayDataOutput out) {
      writeVarint(HEX_COLOR_LITERAL, out);
      out.writeInt(rgb);
    }

    private static final int STRUCT_INITIALIZER = 15; // Varint, struct id, num fields, fields*

    public static void writeStructInitializer(
        int structId, Map<Integer, Writer> fieldInitializerWriters, ByteArrayDataOutput out)
        throws CompilerException {
      writeVarint(STRUCT_INITIALIZER, out);
      writeVarint(structId, out);

      writeVarint(fieldInitializerWriters.size(), out);
      for (Map.Entry<Integer, Writer> entry : fieldInitializerWriters.entrySet()) {
        writeVarint(entry.getKey(), out);
        entry.getValue().write(out);
      }
    }

    // Unary operations: argument follows
    private static void writeUnaryOp(int opCode, Writer argumentWriter, ByteArrayDataOutput out)
        throws CompilerException {
      writeVarint(opCode, out);
      argumentWriter.write(out);
    }

    private static final int BOOLEAN_NEGATION = 20;

    public static void writeBooleanNegation(Writer argumentWriter, ByteArrayDataOutput out)
        throws CompilerException {
      writeUnaryOp(BOOLEAN_NEGATION, argumentWriter, out);
    }

    private static final int POST_INCREMENT = 21;

    public static void writePostIncrement(Writer argumentWriter, ByteArrayDataOutput out)
        throws CompilerException {
      writeUnaryOp(POST_INCREMENT, argumentWriter, out);
    }

    private static final int POST_DECREMENT = 22;

    public static void writePostDecrement(Writer argumentWriter, ByteArrayDataOutput out)
        throws CompilerException {
      writeUnaryOp(POST_DECREMENT, argumentWriter, out);
    }

    // Binary operations: lhs argument, then rhs argument follow
    private static void writeBinaryOp(
        int opCode, Writer lhsWriter, Writer rhsWriter, ByteArrayDataOutput out)
        throws CompilerException {
      writeVarint(opCode, out);
      lhsWriter.write(out);
      rhsWriter.write(out);
    }

    private static final int LOGICAL_AND = 30;

    public static void writeLogicalAnd(Writer lhsWriter, Writer rhsWriter, ByteArrayDataOutput out)
        throws CompilerException {
      writeBinaryOp(LOGICAL_AND, lhsWriter, rhsWriter, out);
    }

    private static final int LOGICAL_OR = 31;

    public static void writeLogicalOr(Writer lhsWriter, Writer rhsWriter, ByteArrayDataOutput out)
        throws CompilerException {
      writeBinaryOp(LOGICAL_OR, lhsWriter, rhsWriter, out);
    }

    private static final int LESS_THAN = 32;

    public static void writeLessThan(Writer lhsWriter, Writer rhsWriter, ByteArrayDataOutput out)
        throws CompilerException {
      writeBinaryOp(LESS_THAN, lhsWriter, rhsWriter, out);
    }

    private static final int EQUALS = 33;

    public static void writeEquals(Writer lhsWriter, Writer rhsWriter, ByteArrayDataOutput out)
        throws CompilerException {
      writeBinaryOp(EQUALS, lhsWriter, rhsWriter, out);
    }

    private static final int ADDITION = 34;

    public static void writeAddition(Writer lhsWriter, Writer rhsWriter, ByteArrayDataOutput out)
        throws CompilerException {
      writeBinaryOp(ADDITION, lhsWriter, rhsWriter, out);
    }

    private static final int SUBTRACTION = 35;

    public static void writeSubtraction(Writer lhsWriter, Writer rhsWriter, ByteArrayDataOutput out)
        throws CompilerException {
      writeBinaryOp(SUBTRACTION, lhsWriter, rhsWriter, out);
    }

    private static final int MULTIPLICATION = 36;

    public static void writeMultiplication(
        Writer lhsWriter, Writer rhsWriter, ByteArrayDataOutput out) throws CompilerException {
      writeBinaryOp(MULTIPLICATION, lhsWriter, rhsWriter, out);
    }

    private static final int DIVISION = 37;

    public static void writeDivision(Writer lhsWriter, Writer rhsWriter, ByteArrayDataOutput out)
        throws CompilerException {
      writeBinaryOp(DIVISION, lhsWriter, rhsWriter, out);
    }

    private static final int ASSIGNMENT = 38;

    public static void writeAssignment(Writer lhsWriter, Writer rhsWriter, ByteArrayDataOutput out)
        throws CompilerException {
      writeBinaryOp(ASSIGNMENT, lhsWriter, rhsWriter, out);
    }
  }

  public static class ContentOpCodes {
    private ContentOpCodes() {}

    private static final int TEXT_CONTENT = 10;

    public static void writeText(Writer expr, ByteArrayDataOutput out) throws CompilerException {
      writeVarint(TEXT_CONTENT, out);
      expr.write(out);
    }

    private static final int WHITESPACE = 11;

    public static void writeWhitespace(ByteArrayDataOutput out) throws CompilerException {
      writeVarint(WHITESPACE, out);
    }

    private static final int COLOR_OPEN = 12;

    public static void writeColorOpen(Writer expr, ByteArrayDataOutput out)
        throws CompilerException {
      writeVarint(COLOR_OPEN, out);
      expr.write(out);
    }

    private static final int URL_OPEN = 13;

    public static void writeUrlOpen(Writer target, ByteArrayDataOutput out)
        throws CompilerException {
      writeVarint(URL_OPEN, out);
      target.write(out);
    }

    private static final int JUMP = 20;

    public static final int JUMP_SIZE;

    static {
      try {
        JUMP_SIZE = capture(o -> writeJump(0, o)).length;
      } catch (CompilerException ex) {
        throw new AssertionError(ex);
      }
    }

    public static void writeJump(int offset, ByteArrayDataOutput out) {
      writeVarint(JUMP, out);
      out.writeInt(offset);
    }

    private static final int CJUMP = 21;

    public static void writeCJump(Writer expr, int offset, ByteArrayDataOutput out)
        throws CompilerException {
      writeVarint(CJUMP, out);
      expr.write(out);
      writeVarint(offset, out);
    }

    private static final int SJUMP = 22;

    @AutoValue
    public abstract static class SJumpEntry implements Writer {
      public abstract int key();

      public abstract int offset();

      @Override
      public final void write(ByteArrayDataOutput out) {
        writeVarint(key(), out);
        writeVarint(offset(), out);
      }

      public static SJumpEntry of(int key, int offset) {
        Verify.verify(offset >= 0, "%d", offset);
        return new AutoValue_Compiler_ContentOpCodes_SJumpEntry(key, offset);
      }
    }

    public static void writeSJump(
        Writer expr, List<SJumpEntry> sJumpEntries, int finalOffset, ByteArrayDataOutput out)
        throws CompilerException {
      writeVarint(SJUMP, out);
      expr.write(out);
      writeVarint(sJumpEntries.size(), out);
      sJumpEntries.forEach(e -> e.write(out));
      writeVarint(finalOffset, out);
    }

    private static final int EXEC = 30;

    public static void writeExec(Writer expr, ByteArrayDataOutput out) throws CompilerException {
      writeVarint(EXEC, out);
      expr.write(out);
    }

    private static final int EXEC_SUB_BLOCK = 31;

    public static void writeExecSubBlock(int subBlockId, ByteArrayDataOutput out)
        throws CompilerException {
      writeVarint(EXEC_SUB_BLOCK, out);
      writeVarint(subBlockId, out);
    }

    enum BBCode {
      BOLD_OPEN(61),
      ITALICS_OPEN(62),
      BOLD_CLOSE(71),
      ITALICS_CLOSE(72),
      COLOR_CLOSE(73),
      URL_CLOSE(74);

      private final int code;

      BBCode(int code) {
        this.code = code;
      }

      public int code() {
        return code;
      }
    }

    public static void writeBBCodeLiteral(BBCode code, ByteArrayDataOutput out) {
      writeVarint(code.code(), out);
    }
  }

  public static class PromptOpCodes {
    private PromptOpCodes() {}

    private static final int PROMPT_OPTION = 50;

    public static void writePromptOption(Writer body, ByteArrayDataOutput out)
        throws CompilerException {
      writeVarint(PROMPT_OPTION, out);
      writeVarintBytes(body, out);
    }

    private static final int PROMPT_GROUP = 51;

    public static void writePromptGroup(
        Writer text, Iterable<Writer> items, ByteArrayDataOutput out) throws CompilerException {
      writeVarint(PROMPT_GROUP, out);

      Compiler.Writer contentWriter =
          o -> {
            writeVarintBytes(text, o);
            for (Writer item : items) {
              item.write(o);
            }
          };
      writeVarintBytes(contentWriter, out);
    }
  }

  public static class InternalOps {
    private InternalOps() {}

    public static void writeVariableAssignment(
        Compiler.Registry registry, String name, Compiler.Writer value, ByteArrayDataOutput out)
        throws CompilerException {
      Compiler.Writer var =
          o -> Compiler.ExprOpCodes.writeVariableReference(registry.getVariableId(name), o);
      Compiler.Writer assign = o -> Compiler.ExprOpCodes.writeAssignment(var, value, o);
      Compiler.ContentOpCodes.writeExec(assign, out);
    }

    public static void writeFunctionCall(
        Compiler.Registry registry,
        String name,
        Iterable<Compiler.Writer> values,
        ByteArrayDataOutput out)
        throws CompilerException {
      Compiler.Writer expr =
          o -> Compiler.ExprOpCodes.writeScriptInvocation(registry.getScriptId(name), values, o);
      Compiler.ContentOpCodes.writeExec(expr, out);
    }

    public static void writeBooleanVariableTest(
        Compiler.Registry registry,
        String name,
        boolean expectedValue,
        int offset,
        ByteArrayDataOutput out)
        throws CompilerException {
      Writer expr1 = o -> ExprOpCodes.writeVariableReference(registry.getVariableId(name), o);
      Writer expr2 = expectedValue ? expr1 : o -> ExprOpCodes.writeBooleanNegation(expr1, o);
      Compiler.ContentOpCodes.writeCJump(expr2, offset, out);
    }
  }

  public static class IndexOpCodes {
    private IndexOpCodes() {}

    private static final int STRUCT_DEFINITION = 1;

    public static void writeStructDefinition(
        Compiler.Registry registry, StructType structType, ByteArrayDataOutput out)
        throws CompilerException {
      writeVarint(STRUCT_DEFINITION, out);
      writeVarint(registry.getStructId(structType), out);
      writeUTF8(structType.typeName(), out);
      writeVarint(structType.fields().size(), out);
      for (StructType.Field field : structType.fields().values()) {
        writeUTF8(field.name(), out);
        if (field.defaultValue().isPresent()) {
          field.defaultValue().get().compile(registry, out);
        } else {
          Expression.IntegerConstant.NEGATIVE_ONE.compile(registry, out);
        }
      }
    }

    private static final int VARIABLE_DEFINITION = 2; // varint id, UTF8 name, Expression
    // (initializer)

    public static void writeVariableDefinition(
        int id, String name, Writer initializer, ByteArrayDataOutput out) throws CompilerException {
      writeVarint(VARIABLE_DEFINITION, out);
      writeVarint(id, out);
      writeUTF8(name, out);
      initializer.write(out);
    }

    private static final int BLOCK_ENTRY = 10; // varint id, UTF8 name, Varint offset, Varint length

    public static void writeBlockEntry(
        int id, String name, int offset, int length, ByteArrayDataOutput out) {
      writeVarint(BLOCK_ENTRY, out);
      writeVarint(id, out);
      writeUTF8(name, out);
      writeVarint(offset, out);
      writeVarint(length, out);
    }

    private static final int SUB_BLOCK_ENTRY = 11;

    public static void writeSubBlockEntry(
        int id, String name, int offset, int length, ByteArrayDataOutput out) {
      writeVarint(SUB_BLOCK_ENTRY, out);
      writeVarint(id, out);
      writeUTF8(name, out);
      writeVarint(offset, out);
      writeVarint(length, out);
    }

    private static final int PROMPT_ENTRY = 12; // varint id, UTF8 name, Varint offset, Varint
    // length

    public static void writePromptEntry(
        int id, String name, int offset, int length, ByteArrayDataOutput out) {
      writeVarint(PROMPT_ENTRY, out);
      writeVarint(id, out);
      writeUTF8(name, out);
      writeVarint(offset, out);
      writeVarint(length, out);
    }

    private static final int SCRIPT_DECLARATION = 13; // varint id, UTF8 name, Varint numArgs

    public static void writeScriptDeclaration(
        int id, String name, int numArgs, ByteArrayDataOutput out) {
      writeVarint(SCRIPT_DECLARATION, out);
      writeVarint(id, out);
      writeUTF8(name, out);
      writeVarint(numArgs, out);
    }

    private static final int BLOCK_DECLARATION = 14;

    public static void writeBlockDeclaration(int id, String name, ByteArrayDataOutput out) {
      writeVarint(BLOCK_DECLARATION, out);
      writeVarint(id, out);
      writeUTF8(name, out);
    }
  }

  // Internal variables.
  public static class InternalVars {
    private InternalVars() {}

    public static final String NEXT_BLOCK_ID = "next_block_id"; // BLOCK_ID
    public static final String CURRENT_PROMPT_ID = "current_prompt_id"; // PROMPT_ID
    public static final String ANY_OPTION_IN_GROUP = "any_option_in_group"; // BOOLEAN

    public static final ImmutableList<String> ALL =
        ImmutableList.of(NEXT_BLOCK_ID, CURRENT_PROMPT_ID, ANY_OPTION_IN_GROUP);
  }

  // Internal scripts.
  public static class InternalScripts {
    private InternalScripts() {}

    // $func(key:STRING, target:BLOCK_ID):VOID
    public static final String SET_KEY_TARGET = "set_key_target";

    public static final ImmutableList<String> ALL = ImmutableList.of(SET_KEY_TARGET);
  }

  @AutoValue
  public abstract static class Registry {
    public abstract LabelRegistry labelRegistry();

    abstract ImmutableMap<String, Integer> blockIds();

    abstract ImmutableMap<AST.Block, Integer> generatedBlockIds();

    public final int getBlockId(AST.Block block) {
      return block.hasId() ? blockIds().get(block.id().name()) : generatedBlockIds().get(block);
    }

    public final int getBlockId(Expression.BlockId blockId) {
      return blockIds().get(blockId.name());
    }

    public final int getBlockId(TypedTag.ExternBlock block) {
      return blockIds().get(block.id().name());
    }

    public final String getBlockName(AST.Block block) {
      return block.hasId() ? block.id().name() : String.format("BLOCK$%d", getBlockId(block));
    }

    abstract ImmutableMap<Integer, Integer> nextBlockIds();

    public final Optional<Integer> getNextBlockId(AST.Block block) {
      return Optional.ofNullable(nextBlockIds().get(getBlockId(block)));
    }

    abstract ImmutableMap<String, Integer> subBlockIds();

    public final int getSubBlockId(AST.SubBlock subBlock) {
      return getSubBlockId(subBlock.id());
    }

    public final int getSubBlockId(Expression.SubBlockId subBlockId) {
      return subBlockIds().get(subBlockId.raw());
    }

    abstract ImmutableMap<String, Integer> promptIds();

    public final int getPromptId(String name) {
      return promptIds().get(name);
    }

    public final int getLocalPromptId(AST.Block block) {
      return promptIds().get(getLocalPromptName(block));
    }

    public final String getLocalPromptName(AST.Block block) {
      return String.format("%s$PROMPT", getBlockName(block));
    }

    abstract ImmutableMap<String, Integer> variableIds();

    public final int getVariableId(String name) {
      return variableIds().get(name);
    }

    abstract ImmutableMap<String, Integer> scriptIds();

    public final int getScriptId(String name) {
      return scriptIds().get(name);
    }

    abstract ImmutableMap<StructType, Integer> structIds();

    public final int getStructId(StructType structType) {
      return structIds().get(structType);
    }

    abstract ImmutableTable<StructType, String, Integer> fieldNumbers();

    public final int getFieldNumber(StructType structType, String fieldName) {
      return fieldNumbers().get(structType, fieldName);
    }

    static Builder builder(LabelRegistry labelRegistry) {
      return new AutoValue_Compiler_Registry.Builder().setLabelRegistry(labelRegistry);
    }

    @AutoValue.Builder
    abstract static class Builder {
      private int nextBlockId = 1;
      private int nextSubBlockId = 1;
      private int nextPromptId = 1;
      private int nextVariableId = 1;
      private int nextScriptId = 1;
      private int nextStructId = 1;

      abstract Builder setLabelRegistry(LabelRegistry labelRegistry);

      abstract ImmutableMap.Builder<String, Integer> blockIdsBuilder();

      abstract ImmutableMap.Builder<AST.Block, Integer> generatedBlockIdsBuilder();

      abstract ImmutableMap.Builder<Integer, Integer> nextBlockIdsBuilder();

      abstract ImmutableMap.Builder<String, Integer> subBlockIdsBuilder();

      abstract ImmutableMap.Builder<String, Integer> promptIdsBuilder();

      abstract ImmutableMap.Builder<String, Integer> variableIdsBuilder();

      abstract ImmutableMap.Builder<String, Integer> scriptIdsBuilder();

      abstract ImmutableMap.Builder<StructType, Integer> structIdsBuilder();

      abstract ImmutableTable.Builder<StructType, String, Integer> fieldNumbersBuilder();

      public final int registerBlock(AST.Block block, Optional<Integer> previousBlock) {
        int id = nextBlockId++;
        String blockId;
        if (block.hasId()) {
          blockId = block.id().name();
        } else {
          blockId = String.format("BLOCK$%d", id);
          generatedBlockIdsBuilder().put(block, id);
        }
        blockIdsBuilder().put(blockId, id);

        if (block.hasPrompt()) {
          String promptId = String.format("%s$PROMPT", blockId);
          promptIdsBuilder().put(promptId, nextPromptId++);
        }

        previousBlock.ifPresent(x -> nextBlockIdsBuilder().put(x, id));

        return id;
      }

      public final void registerBlock(TypedTag.ExternBlock externBlock) {
        int id = nextBlockId++;
        blockIdsBuilder().put(externBlock.id().name(), id);
      }

      public final void registerSubBlock(AST.SubBlock subBlock) {
        int id = nextSubBlockId++;
        subBlockIdsBuilder().put(subBlock.id().raw(), id);
      }

      public final void registerPrompt(AST.Prompt prompt) {
        Verify.verify(prompt.hasId());
        promptIdsBuilder().put(prompt.id().name(), nextPromptId++);
      }

      public final void registerVariable(TypedTag.Define define) {
        variableIdsBuilder().put(define.variable().name(), nextVariableId++);
      }

      public final void registerInternalVariable(String name) {
        variableIdsBuilder().put(name, nextVariableId++);
      }

      public final void registerScript(TypedTag.ExternScript script) {
        scriptIdsBuilder().put(script.functionName(), nextScriptId++);
      }

      public final void registerInternalScript(String name) {
        scriptIdsBuilder().put(name, nextScriptId++);
      }

      public final void registerStruct(StructType structType) {
        structIdsBuilder().put(structType, nextStructId++);
        for (int i = 0; i < structType.fields().size(); i++) {
          fieldNumbersBuilder()
              .put(structType, structType.fields().entrySet().asList().get(i).getKey(), i + 1);
        }
      }

      public abstract Registry build();
    }
  }

  private final ImmutableList<AST> asts;
  private final LabelRegistry labelRegistry;

  private byte[] indexFile;
  private byte[] outFile;

  public Compiler(Collection<AST> asts, LabelRegistry labelRegistry) {
    this.asts = ImmutableList.copyOf(asts);
    this.labelRegistry = labelRegistry;
  }

  private Registry buildRegistry() {
    Registry.Builder builder = Registry.builder(labelRegistry);

    // Internal vars
    InternalVars.ALL.forEach(builder::registerInternalVariable);
    InternalScripts.ALL.forEach(builder::registerInternalScript);

    // Types
    labelRegistry.structTypes().stream().forEach(builder::registerStruct);

    for (AST ast : asts) {
      Optional<Integer> previousBlock = Optional.empty();
      for (AST.Block block : ast.blocks()) {
        previousBlock = Optional.of(builder.registerBlock(block, previousBlock));
      }
      ast.subBlocks().forEach(builder::registerSubBlock);
      ast.prompts().forEach(builder::registerPrompt);

      for (TypedTag tag : ast.atomicDefinitions()) {
        switch (tag.type()) {
          case DEFINE:
            builder.registerVariable(tag.cast());
            break;
          case EXTERN_BLOCK:
            builder.registerBlock(tag.cast());
            break;
          case EXTERN_SCRIPT:
            builder.registerScript(tag.cast());
            break;
          default:
            break; // Ignore
        }
      }
    }

    return builder.build();
  }

  public static byte[] capture(Writer writer) throws CompilerException {
    ByteArrayDataOutput out = ByteStreams.newDataOutput();
    writer.write(out);
    return out.toByteArray();
  }

  public static void writeVarint(int value, ByteArrayDataOutput out) {
    while (value >= 128) {
      out.writeByte((value % 128) | 128);
      value /= 128;
    }
    out.writeByte(value);
  }

  public static void writeVarintBytes(byte[] bytes, ByteArrayDataOutput out) {
    writeVarint(bytes.length, out);
    out.write(bytes);
  }

  public static void writeVarintBytes(Writer bytes, ByteArrayDataOutput out)
      throws CompilerException {
    writeVarintBytes(capture(bytes), out);
  }

  public static void writeUTF8(String value, ByteArrayDataOutput out) {
    writeVarintBytes(value.getBytes(StandardCharsets.UTF_8), out);
  }

  public void compile() throws CompilerException {
    Registry registry = buildRegistry();

    ByteArrayDataOutput indexOut = ByteStreams.newDataOutput();
    ByteArrayDataOutput outOut = ByteStreams.newDataOutput();
    int outIndex = 0;

    // Internals.
    for (String var : InternalVars.ALL) {
      IndexOpCodes.writeVariableDefinition(
          registry.getVariableId(var), var, o -> ExprOpCodes.writeIntegerLiteral(0, o), indexOut);
    }

    for (StructType structType : registry.labelRegistry().structTypes()) {
      IndexOpCodes.writeStructDefinition(registry, structType, indexOut);
    }

    for (AST ast : asts) {
      // 1) Index global definitions
      for (TypedTag tag : ast.atomicDefinitions()) {
        switch (tag.type()) {
          case DEFINE:
            {
              TypedTag.Define define = tag.cast();
              IndexOpCodes.writeVariableDefinition(
                  registry.getVariableId(define.variable().name()),
                  define.variable().name(),
                  define.value().writer(registry),
                  indexOut);
              break;
            }
          case EXTERN_BLOCK:
            {
              TypedTag.ExternBlock externBlock = tag.cast();
              IndexOpCodes.writeBlockDeclaration(
                  registry.getBlockId(externBlock), externBlock.id().name(), indexOut);
              break;
            }
          case EXTERN_SCRIPT:
            {
              TypedTag.ExternScript externScript = tag.cast();
              IndexOpCodes.writeScriptDeclaration(
                  registry.getScriptId(externScript.functionName()),
                  externScript.functionName(),
                  externScript.numArgs(),
                  indexOut);
              break;
            }
          default:
            break; // Ignore
        }
      }

      // 2) Compile and index blocks
      for (AST.Block block : ast.blocks()) {
        byte[] blockBytes = capture(out -> block.compile(registry, out));
        IndexOpCodes.writeBlockEntry(
            registry.getBlockId(block),
            registry.getBlockName(block),
            outIndex,
            blockBytes.length,
            indexOut);
        outOut.write(blockBytes);
        outIndex += blockBytes.length;

        if (block.hasPrompt()) {
          byte[] promptBytes = capture(out -> block.prompt().compile(registry, out));
          IndexOpCodes.writePromptEntry(
              registry.getLocalPromptId(block),
              registry.getLocalPromptName(block),
              outIndex,
              promptBytes.length,
              indexOut);
          outOut.write(promptBytes);
          outIndex += promptBytes.length;
        }
      }

      // 3) Compile and index sub-blocks
      for (AST.SubBlock subBlock : ast.subBlocks()) {
        byte[] subBlockBytes = capture(out -> subBlock.compile(registry, out));
        IndexOpCodes.writeSubBlockEntry(
            registry.getSubBlockId(subBlock),
            subBlock.id().name(),
            outIndex,
            subBlockBytes.length,
            indexOut);
        outOut.write(subBlockBytes);
        outIndex += subBlockBytes.length;
      }

      // 4) Compile and index prompts
      for (AST.Prompt prompt : ast.prompts()) {
        byte[] promptBytes = capture(out -> prompt.compile(registry, out));
        IndexOpCodes.writePromptEntry(
            registry.getPromptId(prompt.id().name()),
            prompt.id().name(),
            outIndex,
            promptBytes.length,
            indexOut);
        outOut.write(promptBytes);
        outIndex += promptBytes.length;
      }
    }

    indexFile = indexOut.toByteArray();
    outFile = outOut.toByteArray();
  }

  public byte[] indexFile() {
    return indexFile;
  }

  public byte[] outFile() {
    return outFile;
  }
}
