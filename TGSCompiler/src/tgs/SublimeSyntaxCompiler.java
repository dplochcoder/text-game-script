package tgs;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.google.auto.value.AutoValue;
import com.google.auto.value.extension.memoized.Memoized;
import com.google.common.base.Verify;
import com.google.common.collect.ComparisonChain;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.ImmutableSetMultimap;
import com.google.common.collect.Interner;
import com.google.common.collect.Interners;
import com.google.common.collect.Lists;
import com.google.common.hash.Hasher;
import com.google.common.hash.Hashing;
import com.google.common.io.BaseEncoding;
import com.google.template.soy.data.SanitizedContent.ContentKind;

public final class SublimeSyntaxCompiler {

  public enum Scope {
    TEXT("text.tgs"),
    COMMENT("comment.tgs"),
    TAG("tag.tgs"),
    BBCODE_TAG("bbcode.tgs"),
    DEFINE_TAG("define_tag.tgs"),
    EXTERN_TAG("extern_tag.tgs"),
    VARIABLE("variable.tgs"),
    CONSTANT_VALUE("value.tgs"),
    ID("id.tgs"),
    ENUM_TYPE("enum_type.tgs"),
    STRUCT_TYPE("struct_type.tgs"),
    SCRIPT("script.tgs"),
    BROKEN("broken_tag.tgs");

    private final String id;

    Scope(String id) {
      this.id = id;
    }

    public String id() {
      return id;
    }
  }

  // Matches the opening bracket, tag id and colon (if allowed/required), then descends into
  // ArgsRule for handling arguments.
  @AutoValue
  public abstract static class TagRule {
    public abstract String tagName();

    public abstract Scope tagScope();

    public abstract Scope tagIdScope();

    public abstract ScopedArgsRule scopedArgsRule();

    public static TagRule bbCodeNoArgs(TypedTag.Type type) {
      return bbCodeWithArgs(type, ArgsRule.NO_MORE_ARGS);
    }

    public static TagRule bbCodeWithArgs(TypedTag.Type type, ArgsRule argsRule) {
      return create(type.tagName().get(), Scope.BBCODE_TAG, Scope.BBCODE_TAG, argsRule);
    }

    public static TagRule defaultNoArgs(TypedTag.Type type) {
      return defaultWithArgs(type, ArgsRule.NO_MORE_ARGS);
    }

    public static TagRule defaultWithArgs(TypedTag.Type type, ArgsRule argsRule) {
      return create(type.tagName().get(), Scope.TAG, Scope.TAG, argsRule);
    }

    public static TagRule defineTagWithArgs(TypedTag.Type type, ArgsRule argsRule) {
      return create(type.tagName().get(), Scope.TAG, Scope.DEFINE_TAG, argsRule);
    }

    public static TagRule externTagWithArgs(TypedTag.Type type, ArgsRule argsRule) {
      return create(type.tagName().get(), Scope.TAG, Scope.EXTERN_TAG, argsRule);
    }

    public static TagRule create(
        String tagName, Scope tagScope, Scope tagIdScope, ArgsRule argsRule) {
      return new AutoValue_SublimeSyntaxCompiler_TagRule(
          tagName, tagScope, tagIdScope, argsRule.scoped(tagScope));
    }
  }

  @AutoValue
  public abstract static class TagValueMatcher {
    public abstract String name();

    public abstract ImmutableList<String> enumValues();

    public final boolean terminal() {
      return this == ARBITRARY_EXPRESSION;
    }

    public static final TagValueMatcher ARBITRARY_EXPRESSION = of("ARBITRARY_EXPRESSION");
    public static final TagValueMatcher SCRIPT_REFERENCE = of("SCRIPT_REFERENCE");
    public static final TagValueMatcher CONSTANT_VALUE = of("CONSTANT_VALUE");
    public static final TagValueMatcher ID = of("ID");
    public static final TagValueMatcher STRING_LITERAL = of("STRING_LITERAL");
    public static final TagValueMatcher HEX_COLOR_LITERAL = of("HEX_COLOR_LITERAL");

    private static TagValueMatcher of(String name) {
      return new AutoValue_SublimeSyntaxCompiler_TagValueMatcher(name, ImmutableList.of());
    }

    private static final Interner<TagValueMatcher> INTERNER = Interners.newStrongInterner();

    public static TagValueMatcher ofEnum(Collection<String> values) {
      return INTERNER.intern(
          new AutoValue_SublimeSyntaxCompiler_TagValueMatcher(
              "ENUM_SHORTHAND",
              values
                  .stream()
                  .sorted(REVERSE_LENGTH_ORDER)
                  .collect(ImmutableList.toImmutableList())));
    }
  }

  @AutoValue
  public abstract static class ScopedTagValueMatcher {

    public abstract Optional<Scope> scope();

    public abstract TagValueMatcher matcher();

    private boolean terminal() {
      return matcher().terminal();
    }

    private static ScopedTagValueMatcher create(Scope scope, TagValueMatcher matcher) {
      return new AutoValue_SublimeSyntaxCompiler_ScopedTagValueMatcher(Optional.of(scope), matcher);
    }

    private static ScopedTagValueMatcher create(TagValueMatcher matcher) {
      return new AutoValue_SublimeSyntaxCompiler_ScopedTagValueMatcher(Optional.empty(), matcher);
    }

    private static ScopedTagValueMatcher createId(Scope scope) {
      return new AutoValue_SublimeSyntaxCompiler_ScopedTagValueMatcher(
          Optional.of(scope), TagValueMatcher.ID);
    }

    public static final ScopedTagValueMatcher ARBITRARY_EXPRESSION =
        create(TagValueMatcher.ARBITRARY_EXPRESSION);

    public static final ScopedTagValueMatcher ID = createId(Scope.ID);
    public static final ScopedTagValueMatcher VARIABLE = createId(Scope.VARIABLE);
    public static final ScopedTagValueMatcher ENUM_TYPE = createId(Scope.ENUM_TYPE);
    public static final ScopedTagValueMatcher ENUM_VALUE = createId(Scope.CONSTANT_VALUE);
    public static final ScopedTagValueMatcher STRUCT_TYPE = createId(Scope.STRUCT_TYPE);
    public static final ScopedTagValueMatcher EXTERN_ID_LIGHT = createId(Scope.EXTERN_TAG);
    public static final ScopedTagValueMatcher EXTERN_ID_DARK = createId(Scope.SCRIPT);

    public static final ScopedTagValueMatcher SCRIPT_REFERENCE =
        create(TagValueMatcher.SCRIPT_REFERENCE);
    public static final ScopedTagValueMatcher CONSTANT_VALUE =
        create(TagValueMatcher.CONSTANT_VALUE);
    public static final ScopedTagValueMatcher STRING_LITERAL =
        create(TagValueMatcher.STRING_LITERAL);
    public static final ScopedTagValueMatcher HEX_COLOR_LITERAL =
        create(TagValueMatcher.HEX_COLOR_LITERAL);

    private static final Interner<ScopedTagValueMatcher> INTERNER = Interners.newStrongInterner();

    public static ScopedTagValueMatcher ofEnum(Collection<String> values) {
      return INTERNER.intern(create(Scope.CONSTANT_VALUE, TagValueMatcher.ofEnum(values)));
    }
  }

  // An ArgsRule is a tree of matcher sets, which describe an individual argument, and are
  // terminated with a leaf node which is either 'no more arguments' or 'arbitrary expression',
  // since 'arbitrary expression' must be the last element in a tag arguments list.
  public static final class ArgsRule {
    @AutoValue
    public abstract static class Node {
      // The lack of a matcher implies an empty node, which allows an args list to be optional.
      public abstract Optional<ScopedTagValueMatcher> matcher();

      public abstract ArgsRule chain();

      private static Node create(Optional<ScopedTagValueMatcher> matcher, ArgsRule chain) {
        if (matcher.map(ScopedTagValueMatcher::terminal).orElse(true))
          Verify.verify(!chain.allowsMoreArgs(), "more arguments after terminal");

        return new AutoValue_SublimeSyntaxCompiler_ArgsRule_Node(matcher, chain);
      }

      private Node then(ArgsRule other) {
        return create(matcher(), chain().then(other));
      }
    }

    // The absence of further children implies a terminal 'no more arguments' node.
    private ImmutableSet<Node> children;
    private boolean repeating = false;

    private ArgsRule(Iterable<Node> children) {
      this.children = ImmutableSet.copyOf(children);
    }

    private ArgsRule(Node... children) {
      this(Arrays.asList(children));
    }

    public ScopedArgsRule scoped(Scope scope) {
      return ScopedArgsRule.create(scope, this);
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (!(o instanceof ArgsRule)) return false;

      ArgsRule that = (ArgsRule) o;

      // Repeating nodes are only equal to themselves.
      if (this.repeating || that.repeating) return false;
      return this.children.equals(that.children);
    }

    @Override
    public int hashCode() {
      return this.children.hashCode();
    }

    private static final Map<ArgsRule, Integer> UNIQUE_ID = new HashMap<>();
    private static int nextUniqueId = 1;

    private int uniqueId = -1;

    public int intUniqueId() {
      if (uniqueId == -1) uniqueId = UNIQUE_ID.computeIfAbsent(this, a -> nextUniqueId++);

      return uniqueId;
    }

    public String uniqueId() {
      return String.format("args_rule_%d", intUniqueId());
    }

    public boolean allowsMoreArgs() {
      return !children.isEmpty();
    }

    public boolean allowsNoMoreArgs() {
      return children.isEmpty() || children.stream().anyMatch(n -> !n.matcher().isPresent());
    }

    public ArgsRule optional() {
      Verify.verify(!allowsNoMoreArgs(), "already optional");

      List<Node> newNodes = Lists.newArrayList(children.asList());
      newNodes.add(Node.create(Optional.empty(), NO_MORE_ARGS));
      return new ArgsRule(newNodes);
    }

    private static ArgsRule merge(ArgsRule rule1, ArgsRule... rest) {
      return new ArgsRule(
          Lists.asList(rule1, rest)
              .stream()
              .flatMap(r -> r.children.stream())
              .collect(ImmutableSet.toImmutableSet()));
    }

    public static ArgsRule of(ScopedTagValueMatcher matcher) {
      return new ArgsRule(Node.create(Optional.of(matcher), NO_MORE_ARGS));
    }

    public ArgsRule or(ArgsRule other) {
      return merge(this, other);
    }

    public ArgsRule then(ArgsRule other) {
      Verify.verify(!repeating, "cannot add to a repeating rule");

      if (children.isEmpty()) return other;

      return new ArgsRule(
          children.stream().map(node -> node.then(other)).collect(ImmutableList.toImmutableList()));
    }

    public ArgsRule repeating() {
      Verify.verify(!repeating, "cannot repeat a repeated rule");
      Verify.verify(!children.isEmpty(), "cannot repeat a NO_MORE_ARGS rule");
      Verify.verify(
          children.stream().anyMatch(n -> !n.matcher().isPresent()), "repeat must have an out");

      // Create a copy.
      ArgsRule newRule = new ArgsRule(children);

      // Append the copy to itself.
      newRule.repeating = true;
      newRule.children =
          newRule
              .children
              .stream()
              .map(node -> node.matcher().isPresent() ? node.then(newRule) : node)
              .collect(ImmutableSet.toImmutableSet());

      return newRule;
    }

    public static final ArgsRule NO_MORE_ARGS = new ArgsRule();
    public static final ArgsRule ARBITRARY_EXPRESSION =
        of(ScopedTagValueMatcher.ARBITRARY_EXPRESSION);

    public static final ArgsRule ID = of(ScopedTagValueMatcher.ID);
    public static final ArgsRule VARIABLE = of(ScopedTagValueMatcher.VARIABLE);
    public static final ArgsRule ENUM_TYPE = of(ScopedTagValueMatcher.ENUM_TYPE);
    public static final ArgsRule ENUM_VALUE = of(ScopedTagValueMatcher.ENUM_VALUE);
    public static final ArgsRule STRUCT_TYPE = of(ScopedTagValueMatcher.STRUCT_TYPE);
    public static final ArgsRule EXTERN_ID_LIGHT = of(ScopedTagValueMatcher.EXTERN_ID_LIGHT);
    public static final ArgsRule EXTERN_ID_DARK = of(ScopedTagValueMatcher.EXTERN_ID_DARK);

    public static final ArgsRule SCRIPT_EXPRESSION =
        of(ScopedTagValueMatcher.SCRIPT_REFERENCE).then(ARBITRARY_EXPRESSION);
    public static final ArgsRule ID_OR_SCRIPT_EXPRESSION = ID.or(SCRIPT_EXPRESSION);

    public static final ArgsRule CONSTANT_VALUE = of(ScopedTagValueMatcher.CONSTANT_VALUE);
    public static final ArgsRule HEX_COLOR_LITERAL = of(ScopedTagValueMatcher.HEX_COLOR_LITERAL);
    public static final ArgsRule STRING_LITERAL = of(ScopedTagValueMatcher.STRING_LITERAL);
  }

  // Input to the compiler, early-derived from tokens.
  @AutoValue
  public abstract static class Input {
    public abstract ImmutableSetMultimap<String, String> enumValues();

    public abstract ImmutableSet<String> structTypeNames();

    public abstract ImmutableSet<String> structFieldNames();

    public abstract ImmutableMap<String, ImmutableList<Expression.TypeLiteral>> scriptTags();

    private static final long HASH_VERSION = 3L;

    private static void hashStringSet(String identifier, Set<String> set, Hasher hasher) {
      hasher.putString("|" + identifier + "|", StandardCharsets.UTF_8);
      hasher.putString(
          set.stream().sorted().collect(Collectors.joining(",")), StandardCharsets.UTF_8);
    }

    private static void hashTypeList(
        String tagName, ImmutableList<Expression.TypeLiteral> types, Hasher hasher) {
      hasher.putString("|" + tagName + "|", StandardCharsets.UTF_8);
      hasher.putString(
          types.stream().map(t -> t.raw()).collect(Collectors.joining(",")),
          StandardCharsets.UTF_8);
    }

    @Memoized
    public String hash() {
      Hasher hasher = Hashing.sha256().newHasher().putLong(HASH_VERSION);

      hashStringSet("STRUCT_TYPES", structTypeNames(), hasher);
      hashStringSet("STRUCT_FIELDS", structFieldNames(), hasher);

      Set<String> enumTypes = new HashSet<>();
      enumValues()
          .asMap()
          .forEach(
              (t, v) ->
                  enumTypes.add(v.stream().sorted().collect(Collectors.joining("|", t + ":", ""))));
      hashStringSet("ENUM_TYPES", enumTypes, hasher);

      hasher.putString("SCRIPT_TAGS", StandardCharsets.UTF_8);
      scriptTags()
          .entrySet()
          .stream()
          .sorted(Comparator.comparing(e -> e.getKey()))
          .forEach(e -> hashTypeList(e.getKey(), e.getValue(), hasher));

      return BaseEncoding.base16().encode(hasher.hash().asBytes()).toLowerCase();
    }

    public static Builder builder() {
      return new AutoValue_SublimeSyntaxCompiler_Input.Builder();
    }

    @AutoValue.Builder
    public abstract static class Builder {
      private final List<TypedTag.DefineEnum> enums = new ArrayList<>();
      private final List<TypedTag.ExternScript> externScripts = new ArrayList<>();
      private final List<TypedTag.ScriptTag> scriptTags = new ArrayList<>();

      public abstract ImmutableSetMultimap.Builder<String, String> enumValuesBuilder();

      public abstract ImmutableSet.Builder<String> structTypeNamesBuilder();

      public abstract ImmutableSet.Builder<String> structFieldNamesBuilder();

      public abstract ImmutableMap.Builder<String, ImmutableList<Expression.TypeLiteral>>
          scriptTagsBuilder();

      public final void addTagOrText(Tokenizer.TagOrText tagOrText) {
        if (!tagOrText.isTag()) return;

        Tokenizer.Tag tag = tagOrText.tag();
        try {
          if (tag.tagName().equals("define_enum")) {
            TypedTag.DefineEnum defineEnum = TypedTag.DefineEnum.parse(tag).cast();
            enumValuesBuilder().putAll(defineEnum.typeName(), defineEnum.values());
            enums.add(defineEnum);
          } else if (tag.tagName().equals("define_struct")) {
            structTypeNamesBuilder().add(tag.arg(0).arg());
          } else if (tag.tagName().equals("field") || tag.tagName().equals("required_field")) {
            structFieldNamesBuilder().add(tag.arg(0).arg());
          } else if (tag.tagName().equals("extern_script")) {
            TypedTag.ExternScript externScript = TypedTag.ExternScript.parse(tag).cast();
            externScripts.add(externScript);
          } else if (tag.tagName().equals("script_tag")) {
            TypedTag.ScriptTag scriptTag = TypedTag.ScriptTag.parse(tag).cast();
            scriptTags.add(scriptTag);
          }
        } catch (CompilerException ignored) {
        }
      }

      abstract Input doBuild();

      private void acceptAll(LabelRegistry labelRegistry, ErrorCollectingValidator astVisitor)
          throws CompilerException {
        enums.forEach(e -> astVisitor.visit(e, null));
        externScripts.forEach(s -> astVisitor.visit(s, null));
        scriptTags.forEach(s -> astVisitor.visit(s, null));

        if (labelRegistry.hasErrors()) {
          throw labelRegistry.errors().get(0);
        }
      }

      public final Input build() throws CompilerException {
        LabelRegistry labelRegistry = new LabelRegistry();
        acceptAll(labelRegistry, labelRegistry);
        acceptAll(labelRegistry, new ExpressionResolver(labelRegistry));
        acceptAll(labelRegistry, new LabelValidator(labelRegistry));

        for (TypedTag.ScriptTag scriptTag : scriptTags) {
          TypedTag.ExternScript scriptDefinition =
              labelRegistry
                  .scriptDefinition(scriptTag.scriptName())
                  .orElseThrow(
                      () ->
                          new CompilerException(
                              scriptTag.tagNamePos(),
                              String.format(
                                  "extern_script '%s' is not defined", scriptTag.scriptName())));
          scriptTagsBuilder().put(scriptTag.definedTagName(), scriptDefinition.argTypes());
        }

        return doBuild();
      }
    }
  }

  private final Input input;

  public SublimeSyntaxCompiler(Input input) {
    this.input = input;
  }

  private static final String INPUT_HASH_PREFIX = "# input_hash: ";

  public boolean needsChange(File f) {
    try {
      for (String line : Files.readAllLines(f.toPath())) {
        if (line.startsWith(INPUT_HASH_PREFIX)) {
          return !line.substring(INPUT_HASH_PREFIX.length()).equals(input.hash());
        }
      }
      return true;
    } catch (Exception ex) {
      return true;
    }
  }

  private static final Comparator<String> REVERSE_LENGTH_ORDER =
      Comparator.comparing(String::length).reversed().thenComparing(String::compareTo);

  private List<?> structTypeNames() {
    return input
        .structTypeNames()
        .stream()
        .sorted(REVERSE_LENGTH_ORDER)
        .collect(ImmutableList.toImmutableList());
  }

  private List<String> structFieldNames() {
    return input
        .structFieldNames()
        .stream()
        .sorted(REVERSE_LENGTH_ORDER)
        .collect(ImmutableList.toImmutableList());
  }

  private List<String> enumTypeNames() {
    return input
        .enumValues()
        .keySet()
        .stream()
        .sorted(REVERSE_LENGTH_ORDER)
        .collect(ImmutableList.toImmutableList());
  }

  private List<?> enumTypes() {
    return input
        .enumValues()
        .asMap()
        .entrySet()
        .stream()
        .sorted(Comparator.comparing(e -> e.getKey(), REVERSE_LENGTH_ORDER))
        .map(
            e ->
                ImmutableMap.of(
                    "typeName",
                    e.getKey(),
                    "values",
                    e.getValue()
                        .stream()
                        .sorted(REVERSE_LENGTH_ORDER)
                        .collect(Collectors.toList())))
        .collect(ImmutableList.toImmutableList());
  }

  private ArgsRule typeRule(Expression.TypeLiteral type) {
    if (type.literalType().valueType().isEnum()) {
      Expression.ValueType.EnumValueType enumType = type.literalType().valueType().cast();
      return ArgsRule.of(
          ScopedTagValueMatcher.ofEnum(input.enumValues().get(enumType.enumType().typeName())));
    } else {
      // TODO: Specific constant types
      return ArgsRule.CONSTANT_VALUE;
    }
  }

  private TagRule scriptTagRule(String tagName, ImmutableList<Expression.TypeLiteral> types) {
    ArgsRule argsRule = ArgsRule.NO_MORE_ARGS;
    for (Expression.TypeLiteral type : types) {
      argsRule = argsRule.then(typeRule(type));
    }
    return TagRule.create(tagName, Scope.TAG, Scope.EXTERN_TAG, argsRule);
  }

  private Stream<TagRule> tagRules() {
    return Stream.concat(
        TypedTag.Type.tagRules().stream(),
        input.scriptTags().entrySet().stream().map(e -> scriptTagRule(e.getKey(), e.getValue())));
  }

  private List<?> tagRulesNoArgs() {
    return tagRules()
        .filter(t -> t.scopedArgsRule().argsRule().allowsNoMoreArgs())
        .map(
            t ->
                ImmutableMap.of(
                    "tagName",
                    t.tagName(),
                    "tagScope",
                    t.tagScope().id(),
                    "tagIdScope",
                    t.tagIdScope().id()))
        .collect(Collectors.toList());
  }

  private static Stream<?> tagRuleWithArgsForTagRule(TagRule tagRule) {
    if (!tagRule.scopedArgsRule().argsRule().allowsMoreArgs()) return Stream.of();

    return Stream.of(
        ImmutableMap.of(
            "tagName",
            tagRule.tagName(),
            "tagScope",
            tagRule.tagScope().id(),
            "tagIdScope",
            tagRule.tagIdScope().id(),
            "argsRule",
            tagRule.scopedArgsRule().uniqueId()));
  }

  private List<?> tagRulesWithArgs() {
    return tagRules().flatMap(t -> tagRuleWithArgsForTagRule(t)).collect(Collectors.toList());
  }

  @AutoValue
  abstract static class ScopedArgsRule implements Comparable<ScopedArgsRule> {
    abstract Scope scope();

    abstract ArgsRule argsRule();

    @Override
    public final int compareTo(ScopedArgsRule other) {
      return ComparisonChain.start()
          .compare(this.argsRule().intUniqueId(), other.argsRule().intUniqueId())
          .compare(this.scope().name(), other.scope().name())
          .result();
    }

    final String uniqueId() {
      return argsRule().uniqueId() + "_" + scope().name();
    }

    public static ScopedArgsRule create(Scope scope, ArgsRule argsRule) {
      return new AutoValue_SublimeSyntaxCompiler_ScopedArgsRule(scope, argsRule);
    }
  }

  private List<?> argsRules() {
    Set<ScopedArgsRule> rules = new HashSet<>();

    // Deterministic BFS for consistency.
    Queue<ScopedArgsRule> toVisit = new ArrayDeque<>();
    tagRules().forEach(t -> toVisit.add(t.scopedArgsRule()));
    while (!toVisit.isEmpty()) {
      ScopedArgsRule rule = toVisit.remove();
      if (rules.add(rule))
        rule.argsRule().children.stream().forEach(n -> toVisit.add(n.chain().scoped(rule.scope())));
    }

    return rules
        .stream()
        .sorted()
        .map(
            r ->
                ImmutableMap.of(
                    "allowsMoreArgs",
                    r.argsRule().allowsMoreArgs(),
                    "allowsNoMoreArgs",
                    r.argsRule().allowsNoMoreArgs(),
                    "closingTagScope",
                    r.scope().id(),
                    "uniqueId",
                    r.uniqueId(),
                    "nodes",
                    r.argsRule()
                        .children
                        .stream()
                        .filter(n -> n.matcher().isPresent())
                        .map(
                            n ->
                                ImmutableMap.of(
                                    "matcher",
                                    n.matcher().get().matcher().name(),
                                    "scope",
                                    n.matcher()
                                        .get()
                                        .scope()
                                        .map(s -> s.id())
                                        .orElse("unspecified.tgs"),
                                    "enumValues",
                                    n.matcher().get().matcher().enumValues(),
                                    "chain",
                                    n.chain().scoped(r.scope()).uniqueId()))
                        .collect(ImmutableList.toImmutableList())))
        .collect(ImmutableList.toImmutableList());
  }

  public String compileRules() throws IOException {
    Map<String, Object> data = new HashMap<>();
    data.put("inputHash", input.hash());
    data.put("structTypeNames", structTypeNames());
    data.put("structFieldNames", structFieldNames());
    data.put("enumTypeNames", enumTypeNames());
    data.put("enumTypes", enumTypes());
    data.put("tagRulesNoArgs", tagRulesNoArgs());
    data.put("tagRulesWithArgs", tagRulesWithArgs());
    data.put("argsRules", argsRules());

    return SublimeSyntaxTemplateHolder.tofu()
        .newRenderer("tgs.sublimeSyntax")
        .setContentKind(ContentKind.TEXT)
        .setData(data)
        .render();
  }
}
