package tgs;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableSet;

public final class LabelRegistry extends ErrorCollectingValidator {
  private static final ImmutableSet<String> RESERVED_WORDS;

  static {
    ImmutableSet.Builder<String> builder = ImmutableSet.builder();
    builder.add("and", "or", "not"); // Operators
    builder.add("key"); // Special type
    builder.add("block", "prompt"); // Namespace qualifiers

    builder.addAll(Compiler.InternalVars.ALL);
    builder.addAll(Compiler.InternalScripts.ALL);

    // Tag names
    for (TypedTag.Type type : TypedTag.Type.values()) {
      type.tagName().ifPresent(builder::add);
    }

    // Type names
    for (Expression.ValueType.Type type : Expression.ValueType.Type.values()) {
      builder.add(type.name().toLowerCase());
    }

    RESERVED_WORDS = builder.build();
  }

  private final Map<String, AST.Block> blocksById = new HashMap<>();
  private final Map<String, TypedTag.ExternBlock> externBlocksById = new HashMap<>();
  private final Map<String, AST.SubBlock> subBlocksById = new HashMap<>();
  private final Map<String, AST.Prompt> promptsById = new HashMap<>();
  private final Map<String, TypedTag.DefineEnum> enumTypesById = new HashMap<>();
  private final Map<String, AST.StructDefinition> structDefinitionsById = new HashMap<>(); // FIXME
  private final Map<String, StructType> structTypesById = new LinkedHashMap<>();
  private final Map<String, TypedTag.Define> variablesById = new HashMap<>();
  private final Map<String, TypedTag.ExternScript> externScriptsByName = new HashMap<>();
  private final Map<String, TypedTag.ScriptTag> scriptTagsByName = new HashMap<>();

  public LabelRegistry() {
    // Register internal types.
    structTypesById.put(StructType.key().typeName(), StructType.key());
  }

  public void addStructType(StructType structType) {
    Preconditions.checkState(
        structTypesById.putIfAbsent(structType.typeName(), structType) == null,
        structType.typeName());
  }

  public Collection<StructType> structTypes() {
    return Collections.unmodifiableCollection(structTypesById.values());
  }

  public Collection<TypedTag.DefineEnum> enumTypes() {
    return Collections.unmodifiableCollection(enumTypesById.values());
  }

  public boolean isBlockDefined(String blockId) {
    return blockDefinition(blockId).isPresent();
  }

  private Optional<Tokenizer.Pos> blockDefinition(String blockId) {
    if (blocksById.containsKey(blockId))
      return Optional.of(blocksById.get(blockId).tag().tagNamePos());
    if (externBlocksById.containsKey(blockId))
      return Optional.of(externBlocksById.get(blockId).tagNamePos());
    return Optional.empty();
  }

  public Optional<AST.SubBlock> getSubBlock(String id) {
    return Optional.ofNullable(subBlocksById.get(id));
  }

  public Optional<AST.Prompt> getPrompt(String id) {
    return Optional.ofNullable(promptsById.get(id));
  }

  public boolean isVariableDefined(String var) {
    return variablesById.containsKey(var);
  }

  public Expression.ValueType getVariableType(String var) throws CompilerException {
    return variablesById.get(var).value().valueType(this);
  }

  public Optional<TypedTag.DefineEnum> getEnumType(String var) {
    return Optional.ofNullable(enumTypesById.get(var));
  }

  public Optional<AST.StructDefinition> getStructDefinition(String name) {
    return Optional.ofNullable(structDefinitionsById.get(name));
  }

  public Optional<StructType> getStructType(String var) {
    return Optional.ofNullable(structTypesById.get(var));
  }

  private Optional<Tokenizer.Pos> variableDefinition(String var) {
    if (variablesById.containsKey(var)) return Optional.of(variablesById.get(var).tagNamePos());
    if (enumTypesById.containsKey(var)) return Optional.of(enumTypesById.get(var).tagNamePos());
    return Optional.empty();
  }

  public Optional<TypedTag.ExternScript> scriptDefinition(String name) {
    return Optional.ofNullable(externScriptsByName.get(name));
  }

  public Optional<TypedTag.ScriptTag> scriptTagDefinition(String name) {
    return Optional.ofNullable(scriptTagsByName.get(name));
  }

  @Override
  public void visitImpl(AST.Block block) {
    super.visitImpl(block);

    if (!block.hasId()) return;
    String id = block.id().name();
    checkReservedWord(id, block.tag().argPos(0));

    Optional<Tokenizer.Pos> previousDefinition = blockDefinition(id);
    if (previousDefinition.isPresent()) {
      logError(block.tag().tagNamePos(), "duplicate block id");
      logError(previousDefinition.get(), "previous id declaration here");
    } else {
      blocksById.put(id, block);
    }
  }

  @Override
  public void visitImpl(AST.StructDefinition structDef) {
    super.visitImpl(structDef);

    AST.StructDefinition prev = structDefinitionsById.get(structDef.typeName());
    if (prev != null) {
      logError(structDef.tag().argPos(0), "duplicate struct definition");
      logError(prev.tag().argPos(0), "previous struct definition here");
    } else {
      structDefinitionsById.put(structDef.typeName(), structDef);
    }
  }

  @Override
  public void visitImpl(AST.SubBlock subBlock) {
    super.visitImpl(subBlock);

    String id = subBlock.id().name();
    checkReservedWord(id, subBlock.tag().argPos(0));

    Optional<AST.SubBlock> previousDefinition = getSubBlock(id);
    if (previousDefinition.isPresent()) {
      logError(subBlock.tag().tagNamePos(), "duplicate sub_block id");
      logError(previousDefinition.get().tag().tagNamePos(), "previous id declaration here");
    } else {
      subBlocksById.put(id, subBlock);
    }
  }

  @Override
  public void visitImpl(AST.Prompt prompt) {
    super.visitImpl(prompt);

    if (!prompt.hasId()) return;
    String id = prompt.id().name();
    checkReservedWord(id, prompt.id().pos());

    if (promptsById.containsKey(id)) {
      logError(prompt.id().pos(), "duplicate prompt id");
      logError(promptsById.get(id).id().pos(), "previous id declaration here");
    } else {
      promptsById.put(id, prompt);
    }
  }

  @Override
  public void visitImpl(TypedTag.Define define) {
    String var = define.variable().name();
    checkReservedWord(var, define.variable().pos());

    Optional<Tokenizer.Pos> previousDefinition = variableDefinition(var);
    if (previousDefinition.isPresent()) {
      logError(define.tagNamePos(), String.format("duplicate variable definition for '%s'", var));
      logError(previousDefinition.get(), "previous definition here");
    } else {
      variablesById.put(var, define);
    }
  }

  @Override
  public void visitImpl(TypedTag.DefineEnum defineEnum) {
    for (Tokenizer.TagArg arg : defineEnum.rawArgs()) {
      checkReservedWord(arg.arg(), arg.pos());
    }

    String typeName = defineEnum.typeName();

    Optional<Tokenizer.Pos> previousDefinition = variableDefinition(typeName);
    if (previousDefinition.isPresent()) {
      logError(
          defineEnum.tagNamePos(),
          String.format("duplicate variable definition for '%s'", typeName));
      logError(previousDefinition.get(), "previous definition here");
    } else {
      enumTypesById.put(typeName, defineEnum);
    }
  }

  @Override
  public void visitImpl(TypedTag.ExternBlock externBlock) {
    String id = externBlock.id().name();
    checkReservedWord(id, externBlock.id().pos());

    Optional<Tokenizer.Pos> previousDefinition = blockDefinition(id);
    if (previousDefinition.isPresent()) {
      logError(externBlock.tagNamePos(), "duplicate block id");
      logError(previousDefinition.get(), "previous id declaration here");
    } else {
      externBlocksById.put(id, externBlock);
    }
  }

  @Override
  public void visitImpl(TypedTag.ExternScript externScript) {
    Optional<String> name = externScript.rawFunctionName();
    if (!name.isPresent()) return; // We'll error later.

    Optional<TypedTag.ExternScript> prev = Optional.ofNullable(externScriptsByName.get(name.get()));
    if (prev.isPresent()) {
      logError(externScript.tagNamePos(), "duplicate script declaration");
      logError(prev.get().tagNamePos(), "previous script declaration here");
    } else {
      externScriptsByName.put(name.get(), externScript);
    }
  }

  @Override
  public void visitImpl(TypedTag.ScriptTag scriptTag) {
    checkReservedWord(scriptTag.definedTagName(), scriptTag.argPos(0));

    Optional<TypedTag.ScriptTag> prev =
        Optional.ofNullable(scriptTagsByName.get(scriptTag.definedTagName()));
    if (prev.isPresent()) {
      logError(scriptTag.tagNamePos(), "duplicate script tag declaration");
      logError(prev.get().tagNamePos(), "previous script tag declaration here");
    } else {
      scriptTagsByName.put(scriptTag.definedTagName(), scriptTag);
    }
  }

  private void checkReservedWord(String word, Tokenizer.Pos pos) {
    if (RESERVED_WORDS.contains(word.toLowerCase()))
      logError(pos, String.format("'%s' is a reserved word", word));
  }
}
