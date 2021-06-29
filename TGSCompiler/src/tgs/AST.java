package tgs;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;

import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterables;
import com.google.common.collect.Streams;
import com.google.common.io.ByteArrayDataOutput;

import tgs.Compiler.Registry;
import tgs.TypedTag.Type;
import tgs.processor.ASTChild;
import tgs.processor.ASTNode;

@ASTNode
public class AST implements AST_ASTNode {
  public interface TypedScopedBuilder<T> {
    void consumeTag(TypedTag in) throws CompilerException;

    void consumeText(Tokenizer.Text in) throws CompilerException;

    void consumeWhitespace(Tokenizer.Whitespace in) throws CompilerException;

    T build() throws CompilerException;
  }

  public static final class TypedScopedBuilderWrapper<T> {
    private final TypedScopedBuilder<T> delegate;
    private final List<T> list;

    private TypedScopedBuilderWrapper(TypedScopedBuilder<T> delegate, List<T> list) {
      this.delegate = delegate;
      this.list = list;
    }

    public static <T> TypedScopedBuilderWrapper<T> of(
        TypedScopedBuilder<T> delegate, List<T> list) {
      return new TypedScopedBuilderWrapper<>(delegate, list);
    }

    public void consumeTag(TypedTag in) throws CompilerException {
      delegate.consumeTag(in);
    }

    public void consumeText(Tokenizer.Text in) throws CompilerException {
      delegate.consumeText(in);
    }

    public void consumeWhitespace(Tokenizer.Whitespace in) throws CompilerException {
      delegate.consumeWhitespace(in);
    }

    public void finish() throws CompilerException {
      list.add(delegate.build());
    }
  }

  @ASTNode
  public static class StructDefinition implements AST_StructDefinition_ASTNode {

    private final TypedTag.DefineStruct tag;
    private final ImmutableList<TypedTag.StructFieldTag> fieldTags;

    private StructDefinition(TypedTag.DefineStruct tag, List<TypedTag.StructFieldTag> fieldTags) {
      this.tag = tag;
      this.fieldTags = ImmutableList.copyOf(fieldTags);
    }

    @ASTChild
    @Override
    public TypedTag.DefineStruct tag() {
      return tag;
    }

    public String typeName() {
      return tag.typeName();
    }

    @ASTChild
    @Override
    public ImmutableList<TypedTag.StructFieldTag> fieldTags() {
      return fieldTags;
    }

    public StructType defineStructType(LabelRegistry labelRegistry) throws CompilerException {
      StructType.Builder builder = StructType.builder(typeName());
      for (TypedTag.StructFieldTag fieldTag : fieldTags) {
        builder.addField(fieldTag.getTypedField(labelRegistry));
      }
      return builder.build();
    }

    private static class Builder implements TypedScopedBuilder<StructDefinition> {
      private final Set<String> usedFieldNames = new HashSet<>();

      private final TypedTag.DefineStruct tag;
      private List<TypedTag.StructFieldTag> fields = new ArrayList<>();

      private Builder(TypedTag.DefineStruct tag) {
        this.tag = tag;
      }

      @Override
      public void consumeTag(TypedTag in) throws CompilerException {
        switch (in.type()) {
          case FIELD:
          case REQUIRED_FIELD:
            {
              TypedTag.StructFieldTag typed = in.cast();
              addFieldName(typed.fieldName(), typed.fieldNamePos());
              fields.add(typed);
              break;
            }
          default:
            throw new CompilerException(
                in.tagNamePos(),
                "only [field] and [required_filed] are allowed in struct definitions");
        }
      }

      private void addFieldName(String name, Tokenizer.Pos pos) throws CompilerException {
        if (!usedFieldNames.add(name)) {
          throw new CompilerException(pos, "duplicate field name in struct");
        }
      }

      @Override
      public void consumeText(Tokenizer.Text in) throws CompilerException {
        throw new CompilerException(in.startPos(), "text is not allowed in struct definitions");
      }

      @Override
      public void consumeWhitespace(Tokenizer.Whitespace whitespace) {}

      @Override
      public StructDefinition build() throws CompilerException {
        if (fields.isEmpty()) {
          throw new CompilerException(tag.tagNamePos(), "struct has no defined fields");
        }

        return new StructDefinition(tag, fields);
      }
    }
  }

  // A sequence of text and if-else constructs.
  // If-else constructs themselves can have content chains.
  // A content-chain may also contain mutational directives, like 'do', or 'face'.
  @ASTNode
  public static class ContentChain implements AST_ContentChain_ASTNode {
    public interface ScopedBuilder {
      void consumeTag(TypedTag in) throws CompilerException;

      void consumeText(Tokenizer.Text in) throws CompilerException;

      void consumeWhitespace(Tokenizer.Whitespace whitespace) throws CompilerException;

      boolean isClosed();

      Link build() throws CompilerException;

      CompilerException unfinished();
    }

    public abstract static class Link implements ASTNodeInterface {
      public enum Type {
        TEXT,
        WHITESPACE,
        TEXT_TAG,
        BBCODE_BLOCK,
        DIRECTIVE_TAG,
        CONDITIONAL,
        SWITCH;
      }

      private final Type type;

      protected Link(Type type) {
        this.type = type;
      }

      public Type type() {
        return type;
      }

      @SuppressWarnings("unchecked")
      public <T extends Link> T cast() {
        return (T) this;
      }

      public abstract void compile(Compiler.Registry registry, ByteArrayDataOutput out)
          throws CompilerException;

      public static class Text extends Link {
        private final Tokenizer.Text text;

        public Text(Tokenizer.Text text) {
          super(Type.TEXT);
          this.text = text;
        }

        public String text() {
          return text.text();
        }

        public Tokenizer.Pos startPos() {
          return text.startPos();
        }

        @Override
        public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
          Compiler.ContentOpCodes.writeText(
              o -> Compiler.ExprOpCodes.writeStringLiteral(text.text(), o), out);
        }

        @Override
        public <V> V accept(ASTVisitor<V> visitor, V value) {
          return text.accept(visitor, value);
        }

        @Override
        public <V> V visitChildren(ASTVisitor<V> visitor, V value) {
          return text.visitChildren(visitor, value);
        }
      }

      public static class Whitespace extends Link {
        private final Tokenizer.Whitespace whitespace;

        public Whitespace(Tokenizer.Whitespace whitespace) {
          super(Type.WHITESPACE);
          this.whitespace = whitespace;
        }

        public Tokenizer.Pos startPos() {
          return whitespace.startPos();
        }

        @Override
        public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
          Compiler.ContentOpCodes.writeWhitespace(out);
        }

        @Override
        public <V> V accept(ASTVisitor<V> visitor, V value) {
          return whitespace.accept(visitor, value);
        }

        @Override
        public <V> V visitChildren(ASTVisitor<V> visitor, V value) {
          return whitespace.visitChildren(visitor, value);
        }
      }

      public static class TextTag extends Link {
        private final TypedTag tag;

        private TextTag(TypedTag tag) {
          super(Type.TEXT_TAG);
          this.tag = tag;
        }

        public TypedTag tag() {
          return tag;
        }

        @Override
        public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
          tag.compile(registry, out);
        }

        @Override
        public <V> V accept(ASTVisitor<V> visitor, V value) {
          return tag.accept(visitor, value);
        }

        @Override
        public <V> V visitChildren(ASTVisitor<V> visitor, V value) {
          return tag.visitChildren(visitor, value);
        }
      }

      public static class BBCodeBlock extends Link {
        private final AST.BBCodeBlock bbCodeBlock;

        private BBCodeBlock(AST.BBCodeBlock bbCodeBlock) {
          super(Type.BBCODE_BLOCK);
          this.bbCodeBlock = bbCodeBlock;
        }

        public AST.BBCodeBlock bbCodeBlock() {
          return bbCodeBlock;
        }

        @Override
        public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
          bbCodeBlock.compile(registry, out);
        }

        @Override
        public <V> V accept(ASTVisitor<V> visitor, V value) {
          return bbCodeBlock.accept(visitor, value);
        }

        @Override
        public <V> V visitChildren(ASTVisitor<V> visitor, V value) {
          return bbCodeBlock.visitChildren(visitor, value);
        }
      }

      public static class DirectiveTag extends Link {
        private final TypedTag tag;

        private DirectiveTag(TypedTag tag) {
          super(Type.DIRECTIVE_TAG);
          this.tag = tag;
        }

        public TypedTag tag() {
          return tag;
        }

        @Override
        public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
          tag.compile(registry, out);
        }

        @Override
        public <V> V accept(ASTVisitor<V> visitor, V value) {
          return tag.accept(visitor, value);
        }

        @Override
        public <V> V visitChildren(ASTVisitor<V> visitor, V value) {
          return tag.visitChildren(visitor, value);
        }
      }

      public static class Conditional extends Link {
        private final AST.Conditional conditional;

        public Conditional(AST.Conditional conditional) {
          super(Type.CONDITIONAL);
          this.conditional = conditional;
        }

        public AST.Conditional conditional() {
          return conditional;
        }

        @Override
        public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
          conditional.compile(registry, out);
        }

        @Override
        public <V> V accept(ASTVisitor<V> visitor, V value) {
          return conditional.accept(visitor, value);
        }

        @Override
        public <V> V visitChildren(ASTVisitor<V> visitor, V value) {
          return conditional.visitChildren(visitor, value);
        }
      }

      public static class Switch extends Link {
        private final AST.Switch switchAst;

        public Switch(AST.Switch switchAst) {
          super(Type.SWITCH);
          this.switchAst = switchAst;
        }

        public AST.Switch switchAst() {
          return switchAst();
        }

        @Override
        public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
          switchAst.compile(registry, out);
        }

        @Override
        public <V> V accept(ASTVisitor<V> visitor, V value) {
          return switchAst.accept(visitor, value);
        }

        @Override
        public <V> V visitChildren(ASTVisitor<V> visitor, V value) {
          return switchAst.visitChildren(visitor, value);
        }
      }
    }

    private final ImmutableList<Link> links;

    private ContentChain(List<Link> links) {
      this.links = ImmutableList.copyOf(links);
    }

    @ASTChild
    @Override
    public ImmutableList<Link> links() {
      return links;
    }

    public void compile(Compiler.Registry registry, ByteArrayDataOutput out)
        throws CompilerException {
      for (Link link : links) {
        link.compile(registry, out);
      }
    }

    private static class Builder implements TypedScopedBuilder<ContentChain> {
      private final ImmutableSet<BBCode> activeCodes;
      private final List<Link> links = new ArrayList<>();
      private ScopedBuilder scopedBuilder = null;

      public Builder() {
        this(ImmutableSet.of());
      }

      public Builder(ImmutableSet<BBCode> activeCodes) {
        this.activeCodes = activeCodes;
      }

      @Override
      public void consumeTag(TypedTag in) throws CompilerException {
        if (scopedBuilder != null) {
          scopedBuilder.consumeTag(in);
          if (scopedBuilder.isClosed()) {
            links.add(scopedBuilder.build());
            scopedBuilder = null;
          }
          return;
        }

        if (in.isBbCode()) {
          scopedBuilder = new AST.BBCodeBlock.Builder(in.cast(), activeCodes);
          return;
        }

        switch (in.type()) {
          case IF:
            scopedBuilder = new AST.Conditional.Builder(in.cast());
            break;
          case CUSTOM_TAG:
          case DO:
          case NO_TEXT:
          case TO:
          case USE:
          case USE_PROMPT:
          case USE_SUB_BLOCK:
            links.add(new Link.DirectiveTag(in));
            break;
          case STRING:
            links.add(new Link.TextTag(in));
            break;
          case SWITCH:
            scopedBuilder = new AST.Switch.Builder(in.cast());
            break;
          default:
            throw unexpectedType(in);
        }
      }

      @Override
      public void consumeText(Tokenizer.Text text) throws CompilerException {
        if (scopedBuilder != null) {
          scopedBuilder.consumeText(text);
        } else {
          links.add(new Link.Text(text));
        }
      }

      @Override
      public void consumeWhitespace(Tokenizer.Whitespace whitespace) throws CompilerException {
        if (scopedBuilder != null) {
          scopedBuilder.consumeWhitespace(whitespace);
        } else {
          links.add(new Link.Whitespace(whitespace));
        }
      }

      @Override
      public ContentChain build() throws CompilerException {
        if (scopedBuilder != null) throw scopedBuilder.unfinished();

        return new ContentChain(links);
      }
    }
  }

  @ASTNode
  public static class BBCodeBlock implements AST_BBCodeBlock_ASTNode {
    private final TypedTag.TypedBBCodeTag openTag;
    private final ContentChain content;
    private final TypedTag.TypedBBCodeTag closeTag;

    private BBCodeBlock(
        TypedTag.TypedBBCodeTag openTag, ContentChain content, TypedTag.TypedBBCodeTag closeTag) {
      this.openTag = openTag;
      this.content = content;
      this.closeTag = closeTag;
    }

    public BBCode bbCode() {
      return openTag.bbCode();
    }

    @ASTChild
    @Override
    public TypedTag.TypedBBCodeTag openTag() {
      return openTag;
    }

    @ASTChild
    @Override
    public ContentChain content() {
      return content;
    }

    @ASTChild
    @Override
    public TypedTag.TypedBBCodeTag closeTag() {
      return closeTag;
    }

    public void compile(Compiler.Registry registry, ByteArrayDataOutput out)
        throws CompilerException {
      openTag.compile(registry, out);
      content.compile(registry, out);
      closeTag.compile(registry, out);
    }

    private static class Builder implements AST.ContentChain.ScopedBuilder {
      private final TypedTag.TypedBBCodeTag openTag;
      private final ContentChain.Builder contentBuilder;
      private TypedTag.TypedBBCodeTag closeTag = null;

      public Builder(TypedTag.TypedBBCodeTag openTag, ImmutableSet<BBCode> activeCodes)
          throws CompilerException {
        checkCanAddCode(openTag, activeCodes);

        this.openTag = openTag;
        this.contentBuilder =
            new ContentChain.Builder(
                Streams.concat(activeCodes.stream(), Arrays.asList(openTag.bbCode()).stream())
                    .collect(ImmutableSet.toImmutableSet()));
      }

      @Override
      public boolean isClosed() {
        return closeTag != null;
      }

      @Override
      public void consumeTag(TypedTag in) throws CompilerException {
        if (contentBuilder.scopedBuilder != null) {
          contentBuilder.consumeTag(in);
          return;
        }

        if (in.isBbCode()) {
          TypedTag.TypedBBCodeTag bbCode = in.cast();
          if (bbCode.isClose()) {
            if (bbCode.bbCode() == openTag.bbCode()) closeTag = bbCode;
            else
              throw new CompilerException(
                  bbCode.tagNamePos(),
                  String.format("mismatched BBCode tag; expected [/%s]", openTag.tagName()));

            return;
          }
        }

        contentBuilder.consumeTag(in);
      }

      @Override
      public void consumeText(Tokenizer.Text text) throws CompilerException {
        contentBuilder.consumeText(text);
      }

      @Override
      public void consumeWhitespace(Tokenizer.Whitespace whitespace) throws CompilerException {
        contentBuilder.consumeWhitespace(whitespace);
      }

      @Override
      public ContentChain.Link build() throws CompilerException {
        Preconditions.checkState(isClosed());
        return new ContentChain.Link.BBCodeBlock(
            new BBCodeBlock(openTag, contentBuilder.build(), closeTag));
      }

      @Override
      public CompilerException unfinished() {
        return new CompilerException(openTag.tagNamePos(), "unclosed BBCode tag");
      }

      private static void checkCanAddCode(
          TypedTag.TypedBBCodeTag openTag, ImmutableSet<BBCode> activeCodes)
          throws CompilerException {
        if (openTag.isClose())
          throw new CompilerException(openTag.tagNamePos(), "unexpected BBCode tag; no open tags");

        switch (openTag.bbCode()) {
          case BOLD:
          case ITALICS:
          case URL:
            if (activeCodes.contains(openTag.bbCode()))
              throw new CompilerException(openTag.tagNamePos(), "redundant BBCode formatting");
            break;
          case COLOR:
            // Nesting color is okay.
            break;
          default:
            throw new AssertionError(openTag.bbCode());
        }
      }
    }
  }

  @ASTNode
  public static class Conditional implements AST_Conditional_ASTNode {
    @ASTNode
    public static class Branch implements AST_Conditional_Branch_ASTNode {
      private final TypedTag tag;
      private final ContentChain content;

      private Branch(TypedTag tag, ContentChain content) {
        this.tag = tag;
        this.content = content;
      }

      @ASTChild
      @Override
      public TypedTag tag() {
        return tag;
      }

      public boolean hasCondition() {
        return tag.type() == TypedTag.Type.ELSE_IF || tag.type() == TypedTag.Type.IF;
      }

      public Expression condition() {
        switch (tag.type()) {
          case IF:
            TypedTag.If ifTag = tag.cast();
            return ifTag.expr();
          case ELSE_IF:
            TypedTag.ElseIf elseIfTag = tag.cast();
            return elseIfTag.expr();
          default:
            throw new AssertionError(tag.type());
        }
      }

      @ASTChild
      @Override
      public ContentChain content() {
        return content;
      }
    }

    // Always size 1 or more.
    private ImmutableList<Branch> branches;
    private final TypedTag.EndIf endIf;

    private Conditional(List<Branch> branches, TypedTag.EndIf endIf) {
      this.branches = ImmutableList.copyOf(branches);
      this.endIf = endIf;
    }

    @ASTChild
    @Override
    public ImmutableList<Branch> branches() {
      return branches;
    }

    @ASTChild
    @Override
    public TypedTag.EndIf endIf() {
      return endIf;
    }

    public boolean hasElseBranch() {
      return !branches.get(branches.size() - 1).hasCondition();
    }

    public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
      // CJUMP $expr JUMP_SIZE
      // JUMP TO_NEXT_BRANCH
      // BRANCH_BODY_1
      // JUMP TO_AFTER_LAST_BRANCH
      // CJUMP $expr JUMP_SIZE
      // ...

      // We serialize the variable parts, compute the offsets, then go back and rewrite the offsets.
      List<Integer> branchStarts = new ArrayList<>();
      List<byte[]> conditionals = new ArrayList<>();
      List<byte[]> branchBodies = new ArrayList<>();

      // There are some extra JUMP instructions in here sometimes but removing them is hard.
      int offset = 0;
      for (Branch branch : branches) {
        if (branch.hasCondition()) {
          conditionals.add(
              Compiler.capture(
                  o ->
                      Compiler.ContentOpCodes.writeCJump(
                          branch.condition().writer(registry),
                          Compiler.ContentOpCodes.JUMP_SIZE,
                          o)));
          offset += Iterables.getLast(conditionals).length;
          offset += Compiler.ContentOpCodes.JUMP_SIZE;
        }

        branchBodies.add(Compiler.capture(o -> branch.content().compile(registry, o)));
        offset += Iterables.getLast(branchBodies).length;

        if (branch.hasCondition()) {
          offset += Compiler.ContentOpCodes.JUMP_SIZE;
        }

        branchStarts.add(offset);
      }

      // Write with offsets computed.
      int end = offset;
      offset = 0;
      for (int i = 0; i < branches.size(); i++) {
        int nextBranch = branchStarts.get(i);

        if (branches.get(i).hasCondition()) {
          out.write(conditionals.get(i));
          offset += conditionals.get(i).length;
          offset += Compiler.ContentOpCodes.JUMP_SIZE;
          Compiler.ContentOpCodes.writeJump(nextBranch - offset, out);
        }

        out.write(branchBodies.get(i));
        offset += branchBodies.get(i).length;

        if (branches.get(i).hasCondition()) {
          offset += Compiler.ContentOpCodes.JUMP_SIZE;
          Compiler.ContentOpCodes.writeJump(end - offset, out);
        }
      }
    }

    private static class Builder implements ContentChain.ScopedBuilder {
      private List<Branch> branches = new ArrayList<>();

      private TypedTag currentTag;
      private Expression currentExpression;
      private ContentChain.Builder contentBuilder = new ContentChain.Builder();
      private TypedTag.EndIf endIf = null;

      public Builder(TypedTag.If tag) {
        currentTag = tag;
        currentExpression = tag.expr();
      }

      @Override
      public boolean isClosed() {
        return endIf != null;
      }

      @Override
      public CompilerException unfinished() {
        return new CompilerException(
            branches.isEmpty() ? currentTag.tagNamePos() : branches.get(0).tag().tagNamePos(),
            "no [end_if] for this [if: ...]");
      }

      @Override
      public void consumeTag(TypedTag in) throws CompilerException {
        Preconditions.checkState(endIf == null);

        if (contentBuilder.scopedBuilder != null) {
          contentBuilder.consumeTag(in);
          return;
        }

        switch (in.type()) {
          case ELSE_IF:
          case ELSE:
          case END_IF:
            closeBranch(in);
            break;
          case PROMPT:
            throw new CompilerException(
                in.tagNamePos(),
                "cannot define a [prompt] inside an [if] block.  Use [use_prompt: ...] instead");
          default:
            // Add it to the branch
            contentBuilder.consumeTag(in);
            break;
        }
      }

      @Override
      public void consumeText(Tokenizer.Text text) throws CompilerException {
        contentBuilder.consumeText(text);
      }

      @Override
      public void consumeWhitespace(Tokenizer.Whitespace whitespace) throws CompilerException {
        contentBuilder.consumeWhitespace(whitespace);
      }

      private void closeBranch(TypedTag continuation) throws CompilerException {
        branches.add(new Branch(currentTag, contentBuilder.build()));
        contentBuilder = new ContentChain.Builder();

        if (continuation.type() == TypedTag.Type.END_IF) {
          endIf = continuation.cast();
          return;
        }

        if (currentExpression == null)
          throw new CompilerException(
              continuation.tagNamePos(),
              String.format(
                  "[%s] cannot follow [else], only [if: ...] or [else_if: ...]",
                  continuation.tagName()));

        currentTag = continuation;
        switch (continuation.type()) {
          case ELSE_IF:
            TypedTag.ElseIf elseIf = continuation.cast();
            currentExpression = elseIf.expr();
            break;
          case ELSE:
            currentExpression = null;
            break;
          default:
            throw unexpectedType(continuation);
        }
      }

      @Override
      public ContentChain.Link build() throws CompilerException {
        Preconditions.checkState(endIf != null);
        return new ContentChain.Link.Conditional(new Conditional(branches, endIf));
      }
    }
  }

  @ASTNode
  public static class Switch implements AST_Switch_ASTNode {
    @ASTNode
    public static class Case implements AST_Switch_Case_ASTNode {
      private final TypedTag tag;
      private final ContentChain content;

      private Case(TypedTag tag, ContentChain content) {
        this.tag = tag;
        this.content = content;
      }

      public boolean isDefault() {
        return tag.type() == TypedTag.Type.DEFAULT;
      }

      public TypedTag.Case caseTag() {
        return tag.cast();
      }

      public TypedTag.Default defaultTag() {
        return tag.cast();
      }

      @ASTChild
      @Override
      public TypedTag tag() {
        return tag;
      }

      @ASTChild
      @Override
      public ContentChain content() {
        return content;
      }
    }

    // At least one, at most one default.
    private final TypedTag.Switch switchTag;
    private final ImmutableList<Case> nonDefaultCases;
    private final Optional<Case> defaultCase;
    private final TypedTag.EndSwitch endSwitch;

    private Switch(
        TypedTag.Switch switchTag,
        List<Case> nonDefaultCases,
        Optional<Case> defaultCase,
        TypedTag.EndSwitch endSwitch) {
      this.switchTag = switchTag;
      this.nonDefaultCases = ImmutableList.copyOf(nonDefaultCases);
      this.defaultCase = defaultCase;
      this.endSwitch = endSwitch;
    }

    @ASTChild
    @Override
    public TypedTag.Switch switchTag() {
      return switchTag;
    }

    public ImmutableList<Case> nonDefaultCases() {
      return nonDefaultCases;
    }

    @ASTChild
    @Override
    public Stream<Case> allCases() {
      return Streams.concat(nonDefaultCases.stream(), Streams.stream(defaultCase));
    }

    public boolean hasDefaultCase() {
      return defaultCase.isPresent();
    }

    public Case defaultCase() {
      return defaultCase.get();
    }

    @ASTChild
    @Override
    public TypedTag.EndSwitch endSwitch() {
      return endSwitch;
    }

    public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
      // SJUMP $expr k1 c1 k2 c2 k3 c3 ... default_case_or_end_switch
      // CASE_1
      // JUMP END_SWITCH
      // CASE_2
      // JUMP END_SWITCH
      // ...
      Expression.ValueType.EnumValueType enumValueType =
          switchTag.expr().valueType(registry.labelRegistry()).cast();
      TypedTag.DefineEnum definition = enumValueType.enumType();

      // We serialize the variable parts, compute the offsets, then go back and rewrite the offsets.
      List<Compiler.ContentOpCodes.SJumpEntry> sJumpEntries = new ArrayList<>();
      List<byte[]> caseBodies = new ArrayList<>();
      byte[] defaultCaseBody = new byte[0];

      int offset = 0;
      for (Case caseAst : nonDefaultCases()) {
        for (Expression.EnumValueLiteral enumValue : caseAst.caseTag().enumCases(definition)) {
          sJumpEntries.add(Compiler.ContentOpCodes.SJumpEntry.of(enumValue.intValue(), offset));
        }

        caseBodies.add(Compiler.capture(o -> caseAst.content().compile(registry, o)));
        offset += Iterables.getLast(caseBodies).length;
        offset += Compiler.ContentOpCodes.JUMP_SIZE;
      }

      int defaultCaseOffset = offset;
      if (hasDefaultCase()) {
        defaultCaseBody = Compiler.capture(o -> defaultCase().content().compile(registry, o));
        offset += defaultCaseBody.length;
      }
      int end = offset;

      // Write with offsets computed.
      Compiler.ContentOpCodes.writeSJump(
          o -> switchTag().expr().compile(registry, o), sJumpEntries, defaultCaseOffset, out);

      offset = 0;
      for (byte[] buf : caseBodies) {

        out.write(buf);
        offset += buf.length;
        offset += Compiler.ContentOpCodes.JUMP_SIZE;
        Compiler.ContentOpCodes.writeJump(end - offset, out);
      }
      if (hasDefaultCase()) {
        out.write(defaultCaseBody);
      }
    }

    private static class Builder implements ContentChain.ScopedBuilder {
      private List<Case> cases = new ArrayList<>();
      private Case defaultCase = null;

      private final TypedTag.Switch switchTag;
      private TypedTag currentTag = null;
      private ContentChain.Builder contentBuilder = null;
      private TypedTag.EndSwitch endSwitch = null;

      public Builder(TypedTag.Switch switchTag) {
        this.switchTag = switchTag;
      }

      @Override
      public boolean isClosed() {
        return endSwitch != null;
      }

      @Override
      public CompilerException unfinished() {
        return new CompilerException(
            switchTag.tagNamePos(), "no [end_switch] for this [switch: ...]");
      }

      @Override
      public void consumeTag(TypedTag in) throws CompilerException {
        Preconditions.checkState(endSwitch == null);

        if (contentBuilder != null && contentBuilder.scopedBuilder != null) {
          contentBuilder.consumeTag(in);
          return;
        }

        switch (in.type()) {
          case CASE:
          case DEFAULT:
          case END_SWITCH:
            closeCase(in);
            break;
          case PROMPT:
            throw new CompilerException(
                in.tagNamePos(),
                "cannot define a [prompt] inside a [switch] block.  Use [use_prompt: ...] instead");
          default:
            // Add it to the case
            if (contentBuilder == null) {
              throw new CompilerException(
                  in.tagNamePos(), "tag not allowed here; expected [case] or [default] first.");
            }

            contentBuilder.consumeTag(in);
            break;
        }
      }

      @Override
      public void consumeText(Tokenizer.Text text) throws CompilerException {
        if (contentBuilder == null) {
          throw new CompilerException(
              text.startPos(), "Content is not allowed between a [switch] and its first [case]");
        }

        contentBuilder.consumeText(text);
      }

      @Override
      public void consumeWhitespace(Tokenizer.Whitespace whitespace) throws CompilerException {
        if (contentBuilder != null) {
          contentBuilder.consumeWhitespace(whitespace);
        }
      }

      private void closeCase(TypedTag continuation) throws CompilerException {
        if (currentTag != null) {
          Case newCase = new Case(currentTag, contentBuilder.build());
          switch (currentTag.type()) {
            case CASE:
              cases.add(newCase);
              break;
            case DEFAULT:
              defaultCase = newCase;
              break;
            default:
              throw unexpectedType(currentTag);
          }
        }

        switch (continuation.type()) {
          case DEFAULT:
            if (defaultCase != null) {
              throw new CompilerException(
                  continuation.tagNamePos(),
                  "[switch] statements can only have one [default] case");
            }
            // fallthrough intended
          case CASE:
            currentTag = continuation;
            contentBuilder = new ContentChain.Builder();
            break;
          case END_SWITCH:
            endSwitch = continuation.cast();
            return;
          default:
            throw unexpectedType(continuation);
        }
      }

      @Override
      public ContentChain.Link build() throws CompilerException {
        Preconditions.checkState(endSwitch != null);
        return new ContentChain.Link.Switch(
            new Switch(switchTag, cases, Optional.ofNullable(defaultCase), endSwitch));
      }
    }
  }

  @ASTNode
  public static class Block implements AST_Block_ASTNode {
    private final TypedTag.Block tag;
    private final ContentChain content;
    private final Optional<Prompt> prompt;

    private Block(TypedTag.Block tag, ContentChain content, Optional<Prompt> prompt) {
      this.tag = tag;
      this.content = content;
      this.prompt = prompt;
    }

    @ASTChild
    @Override
    public TypedTag.Block tag() {
      return tag;
    }

    public boolean hasId() {
      return tag.hasId();
    }

    public Expression.BlockId id() {
      return tag.id();
    }

    @ASTChild
    @Override
    public ContentChain content() {
      return content;
    }

    public boolean hasPrompt() {
      return prompt.isPresent();
    }

    public Prompt prompt() {
      return prompt.get();
    }

    @ASTChild
    @Override
    public Optional<Prompt> optPrompt() {
      return prompt;
    }

    public void compile(Compiler.Registry registry, ByteArrayDataOutput out)
        throws CompilerException {
      // Set the next block id as the default.
      int next = registry.getNextBlockId(this).orElse(0);
      Compiler.InternalOps.writeVariableAssignment(
          registry,
          Compiler.InternalVars.NEXT_BLOCK_ID,
          o -> Compiler.ExprOpCodes.writeIntegerLiteral(next, o),
          out);

      content.compile(registry, out);

      // Set the auto-gen prompt id if we have one.
      if (hasPrompt()) {
        Compiler.InternalOps.writeVariableAssignment(
            registry,
            Compiler.InternalVars.CURRENT_PROMPT_ID,
            o -> Compiler.ExprOpCodes.writeIntegerLiteral(registry.getLocalPromptId(this), o),
            out);
      }
    }

    private static class Builder implements TypedScopedBuilder<Block> {
      private final TypedTag.Block tag;
      private ContentChain.Builder contentBuilder = new ContentChain.Builder();
      private ContentChain content = null;
      private Prompt.Builder promptBuilder = null;

      public Builder(TypedTag.Block tag) {
        this.tag = tag;
      }

      @Override
      public void consumeTag(TypedTag in) throws CompilerException {
        if (promptBuilder != null) {
          promptBuilder.consumeTag(in);
          return;
        }

        switch (in.type()) {
          case PROMPT:
            if (promptBuilder != null)
              throw new CompilerException(
                  in.tagNamePos(), "only one [prompt] allowed in a [block]");

            content = contentBuilder.build();
            contentBuilder = null;
            promptBuilder = new Prompt.Builder(in.cast());
            break;
          case OPTION:
          case OPTION_DEFAULT:
          case OPTION_IF:
          case OPTION_NONE:
            throw new CompilerException(in.tagNamePos(), "option tags require a [prompt]");
          default:
            contentBuilder.consumeTag(in);
            break;
        }
      }

      @Override
      public void consumeText(Tokenizer.Text text) throws CompilerException {
        if (promptBuilder != null) promptBuilder.consumeText(text);
        else contentBuilder.consumeText(text);
      }

      @Override
      public void consumeWhitespace(Tokenizer.Whitespace whitespace) throws CompilerException {
        if (promptBuilder != null) promptBuilder.consumeWhitespace(whitespace);
        else contentBuilder.consumeWhitespace(whitespace);
      }

      @Override
      public Block build() throws CompilerException {
        return new Block(
            tag,
            content == null ? contentBuilder.build() : content,
            Optional.ofNullable(promptBuilder == null ? null : promptBuilder.build()));
      }
    }
  }

  @ASTNode
  public static class SubBlock implements AST_SubBlock_ASTNode {
    private final TypedTag.SubBlock tag;
    private final ContentChain content;

    private SubBlock(TypedTag.SubBlock tag, ContentChain content) {
      this.tag = tag;
      this.content = content;
    }

    @ASTChild
    @Override
    public TypedTag.SubBlock tag() {
      return tag;
    }

    public Expression.SubBlockId id() {
      return tag.id();
    }

    @ASTChild
    @Override
    public ContentChain content() {
      return content;
    }

    public void compile(Compiler.Registry registry, ByteArrayDataOutput out)
        throws CompilerException {
      content.compile(registry, out);
    }

    private static class Builder implements TypedScopedBuilder<SubBlock> {
      private final TypedTag.SubBlock tag;
      private ContentChain.Builder contentBuilder = new ContentChain.Builder();

      public Builder(TypedTag.SubBlock tag) {
        this.tag = tag;
      }

      @Override
      public void consumeTag(TypedTag in) throws CompilerException {
        switch (in.type()) {
          case TO:
          case USE_PROMPT:
            throw new CompilerException(
                in.tagNamePos(), String.format("[%s] is not allowed in sub-blocks", in.tagName()));
          default:
            contentBuilder.consumeTag(in);
            break;
        }
      }

      @Override
      public void consumeText(Tokenizer.Text text) throws CompilerException {
        contentBuilder.consumeText(text);
      }

      @Override
      public void consumeWhitespace(Tokenizer.Whitespace whitespace) throws CompilerException {
        contentBuilder.consumeWhitespace(whitespace);
      }

      @Override
      public SubBlock build() throws CompilerException {
        return new SubBlock(tag, contentBuilder.build());
      }
    }
  }

  @ASTNode
  public static class Prompt implements AST_Prompt_ASTNode {
    // The number of rows that may potentially be displayed.
    // In all levels, 'maximum' should be at least two.
    private static class OptionCounts {
      public int minimum = 0;
      public int maximum = 0;
    }

    public interface OptionOrOptionGroup extends ASTNodeInterface {
      boolean isGroup();

      ContentChain content();

      void compile(Compiler.Registry registry, ByteArrayDataOutput out) throws CompilerException;
    }

    @ASTNode
    public static final class Option implements OptionOrOptionGroup, AST_Prompt_Option_ASTNode {
      private final TypedTag tag;
      private final ContentChain content;

      private Option(TypedTag tag, ContentChain content) {
        this.tag = tag;
        this.content = content;
      }

      @Override
      public boolean isGroup() {
        return false;
      }

      @ASTChild
      @Override
      public TypedTag tag() {
        return tag;
      }

      @ASTChild
      @Override
      public ContentChain content() {
        return content;
      }

      @Override
      public void compile(Registry registry, ByteArrayDataOutput out) throws CompilerException {
        Compiler.Writer body = o -> content.compile(registry, o);
        switch (tag.type()) {
          case OPTION:
            {
              Compiler.PromptOpCodes.writePromptOption(body, out);
              break;
            }
          case OPTION_IF:
            {
              TypedTag.OptionIf optionIf = tag.cast();
              Compiler.Writer negTest =
                  o ->
                      Compiler.ExprOpCodes.writeBooleanNegation(
                          optionIf.expr().writer(registry), o);
              byte[] optBytes =
                  Compiler.capture(o -> Compiler.PromptOpCodes.writePromptOption(body, o));
              Compiler.ContentOpCodes.writeCJump(negTest, optBytes.length, out);
              out.write(optBytes);
              break;
            }
          case OPTION_DEFAULT:
            {
              byte[] optBytes =
                  Compiler.capture(o -> Compiler.PromptOpCodes.writePromptOption(body, o));
              Compiler.InternalOps.writeBooleanVariableTest(
                  registry, Compiler.InternalVars.ANY_OPTION_IN_GROUP, true, optBytes.length, out);
              out.write(optBytes);
              break;
            }
          case OPTION_NONE:
            {
              byte[] optBytes = Compiler.capture(body);
              Compiler.InternalOps.writeBooleanVariableTest(
                  registry, Compiler.InternalVars.ANY_OPTION_IN_GROUP, true, optBytes.length, out);
              out.write(optBytes);
              break;
            }
          default:
            throw new AssertionError(tag.type());
        }
      }

      private static final class Builder {
        private final TypedTag tag;
        private final ContentChain.Builder contentBuilder = new ContentChain.Builder();

        public Builder(TypedTag tag) {
          this.tag = tag;
        }

        public void consumeTag(TypedTag in) throws CompilerException {
          if (tag.type() == TypedTag.Type.OPTION_NONE) {
            switch (in.type()) {
              case TO:
                break;
              default:
                throw new CompilerException(
                    in.tagNamePos(),
                    String.format("[%s] tag not allowed in [option_none]", in.tagName()));
            }
          } else {
            switch (in.type()) {
              case NO_TEXT:
                throw new CompilerException(
                    in.tagNamePos(),
                    String.format("[%s] tag not allowed in [%s]", in.tagName(), tag.tagName()));
              default:
                break;
            }
          }
          contentBuilder.consumeTag(in);
        }

        public void consumeText(Tokenizer.Text text) throws CompilerException {
          if (tag.type() == TypedTag.Type.OPTION_NONE)
            throw new CompilerException(text.startPos(), "text is not allowed in [option_none]");
          contentBuilder.consumeText(text);
        }

        public void consumeWhitespace(Tokenizer.Whitespace whitespace) throws CompilerException {
          if (tag.type() != TypedTag.Type.OPTION_NONE) {
            contentBuilder.consumeWhitespace(whitespace);
          }
        }

        public Option build(OptionCounts counts) throws CompilerException {
          switch (tag.type()) {
            case OPTION:
              counts.minimum++;
              // fall-through
            case OPTION_IF:
              counts.maximum++;
              break;
            case OPTION_DEFAULT:
            case OPTION_NONE:
              if (counts.minimum == 0) counts.minimum++;
              else if (counts.maximum == 0)
                throw new CompilerException(
                    tag.tagNamePos(),
                    String.format(
                        "[%s] will always be shown here; did you mean to use [option]?",
                        tag.tagName()));
              else
                throw new CompilerException(
                    tag.tagNamePos(),
                    String.format(
                        "[%s] will never be shown because a preceding [option] or [option_default] supplants it",
                        tag.tagName()));
              break;
            default:
              throw new AssertionError(tag.type());
          }

          return new Option(tag, contentBuilder.build());
        }
      }
    }

    @ASTNode
    public static final class OptionGroup
        implements OptionOrOptionGroup, AST_Prompt_OptionGroup_ASTNode {
      private final TypedTag openTag;
      private final ContentChain content;
      private final ImmutableList<OptionOrOptionGroup> options;
      private final TypedTag.EndGroup endTag;

      private OptionGroup(
          TypedTag openTag,
          ContentChain content,
          List<OptionOrOptionGroup> options,
          TypedTag.EndGroup endTag) {
        this.openTag = openTag;
        this.content = content;
        this.options = ImmutableList.copyOf(options);
        this.endTag = endTag;
      }

      @Override
      public boolean isGroup() {
        return true;
      }

      public boolean isConditional() {
        return openTag.type() == Type.OPTION_GROUP_IF;
      }

      @ASTChild
      @Override
      public TypedTag openTag() {
        return openTag;
      }

      @ASTChild
      @Override
      public ContentChain content() {
        return content;
      }

      @ASTChild
      @Override
      public ImmutableList<OptionOrOptionGroup> options() {
        return options;
      }

      @ASTChild
      @Override
      public TypedTag.EndGroup endTag() {
        return endTag;
      }

      @Override
      public void compile(Compiler.Registry registry, ByteArrayDataOutput out)
          throws CompilerException {

        // Render much the same as a prompt.
        List<Compiler.Writer> itemWriters = new ArrayList<>();
        options().forEach(opt -> itemWriters.add(o -> opt.compile(registry, o)));
        Compiler.Writer mainWriter =
            o ->
                Compiler.PromptOpCodes.writePromptGroup(
                    o2 -> content().compile(registry, o2), itemWriters, o);

        if (isConditional()) {
          TypedTag.OptionGroupIf optionGroupIf = openTag.cast();
          Compiler.Writer negTest =
              o ->
                  Compiler.ExprOpCodes.writeBooleanNegation(
                      optionGroupIf.expr().writer(registry), o);
          byte[] optBytes = Compiler.capture(mainWriter);
          Compiler.ContentOpCodes.writeCJump(negTest, optBytes.length, out);
          out.write(optBytes);

        } else {
          mainWriter.write(out);
        }
      }

      private static final class Builder {
        private final TypedTag openTag;
        private ContentChain.Builder contentBuilder = new ContentChain.Builder();
        private ContentChain content = null;
        private final List<OptionOrOptionGroup> options = new ArrayList<>();
        private final OptionCounts subCounts = new OptionCounts();

        private Option.Builder optionBuilder = null;
        private OptionGroup.Builder optionGroupBuilder = null;
        private TypedTag.EndGroup endTag = null;

        public Builder(TypedTag openTag) {
          this.openTag = openTag;
        }

        private boolean isConditional() {
          return openTag.type() == Type.OPTION_GROUP_IF;
        }

        public void consumeTag(TypedTag in) throws CompilerException {
          if (optionGroupBuilder != null) {
            optionGroupBuilder.consumeTag(in);
            if (optionGroupBuilder.isClosed()) {
              options.add(optionGroupBuilder.build(subCounts));
              optionGroupBuilder = null;
            }
            return;
          }

          switch (in.type()) {
            case END_GROUP:
              closeUnit();
              endTag = in.cast();
              return;
            case OPTION:
            case OPTION_DEFAULT:
            case OPTION_IF:
              closeUnit();
              optionBuilder = new Option.Builder(in.cast());
              return;
            case OPTION_GROUP:
            case OPTION_GROUP_IF:
              closeUnit();
              optionGroupBuilder = new OptionGroup.Builder(in);
              return;
            case OPTION_NONE:
              throw new CompilerException(
                  in.tagNamePos(), "[option_none] is not allowed inside an [option_group]");
            default:
              break;
          }

          if (contentBuilder != null) {
            switch (in.type()) {
              case CUSTOM_TAG:
              case DO:
              case NO_TEXT:
              case TO:
                throw new CompilerException(
                    in.tagNamePos(),
                    String.format("[%s] is not allowed in [option_group] text", in.tagName()));
              default:
                break;
            }
            contentBuilder.consumeTag(in);
          } else if (optionBuilder != null) optionBuilder.consumeTag(in);
          else
            throw new CompilerException(in.tagNamePos(), "unexpected tag; expected another option");
        }

        public void consumeText(Tokenizer.Text text) throws CompilerException {
          if (contentBuilder != null) contentBuilder.consumeText(text);
          else if (optionBuilder != null) optionBuilder.consumeText(text);
          else if (optionGroupBuilder != null) optionGroupBuilder.consumeText(text);
          else
            throw new CompilerException(
                text.startPos(),
                "found text in an [option_group], but without an [option] or [option_group]");
        }

        public void consumeWhitespace(Tokenizer.Whitespace whitespace) throws CompilerException {
          if (contentBuilder != null) contentBuilder.consumeWhitespace(whitespace);
          else if (optionBuilder != null) optionBuilder.consumeWhitespace(whitespace);
          else if (optionGroupBuilder != null) optionGroupBuilder.consumeWhitespace(whitespace);
        }

        private void closeUnit() throws CompilerException {
          if (contentBuilder != null) {
            content = contentBuilder.build();
            contentBuilder = null;
          } else if (optionBuilder != null) {
            options.add(optionBuilder.build(subCounts));
            optionBuilder = null;
          } else if (optionGroupBuilder != null) {
            options.add(optionGroupBuilder.build(subCounts));
            optionGroupBuilder = null;
          }
        }

        public boolean isClosed() {
          return endTag != null;
        }

        public OptionGroup build(OptionCounts counts) throws CompilerException {
          closeUnit();

          if (endTag == null)
            throw new CompilerException(
                openTag.tagNamePos(), "missing [end_group] for this [option_group]");
          if (subCounts.maximum < 2)
            throw new CompilerException(
                openTag.tagNamePos(),
                "[option_group] should have at least two independent top-level options");

          if (subCounts.minimum > 0 && !isConditional()) counts.minimum++;
          counts.maximum++;

          return new OptionGroup(openTag, content, options, endTag);
        }
      }
    }

    private final TypedTag.Prompt tag;
    private final ContentChain promptContent;
    private final ImmutableList<OptionOrOptionGroup> options;

    private Prompt(
        TypedTag.Prompt tag, ContentChain promptContent, List<OptionOrOptionGroup> options) {
      this.tag = tag;
      this.promptContent = promptContent;
      this.options = ImmutableList.copyOf(options);
    }

    @ASTChild
    @Override
    public TypedTag.Prompt tag() {
      return tag;
    }

    public boolean hasId() {
      return tag.hasId();
    }

    public Expression.PromptId id() {
      return tag.id();
    }

    @ASTChild
    @Override
    public ContentChain promptContent() {
      return promptContent;
    }

    @ASTChild
    @Override
    public ImmutableList<OptionOrOptionGroup> options() {
      return options;
    }

    public void compile(Compiler.Registry registry, ByteArrayDataOutput out)
        throws CompilerException {
      // First render content.
      Compiler.writeVarintBytes(o -> promptContent().compile(registry, o), out);

      // Then render options.
      // [option_none] will appear as a non-option, its directives will be appended directly to
      // the prompt body. The engine manages the 'any_option_in_group' boolean, we only query it.
      for (OptionOrOptionGroup optionOrOptionGroup : options()) {
        optionOrOptionGroup.compile(registry, out);
      }
    }

    private static class Builder implements TypedScopedBuilder<Prompt> {
      private final TypedTag.Prompt tag;
      private ContentChain.Builder contentBuilder = new ContentChain.Builder();
      private ContentChain content = null;
      private final List<OptionOrOptionGroup> options = new ArrayList<>();
      private final OptionCounts subCounts = new OptionCounts();

      private Option.Builder optionBuilder = null;
      private OptionGroup.Builder optionGroupBuilder = null;

      public Builder(TypedTag.Prompt tag) {
        this.tag = tag;
      }

      @Override
      public void consumeTag(TypedTag in) throws CompilerException {
        switch (in.type()) {
          case DO:
          case URL_OPEN:
          case USE:
          case USE_PROMPT:
            throw new CompilerException(
                in.tagNamePos(),
                String.format("tag [%s] not allowed inside [prompt] or [option]s", in.tagName()));
          default:
            break;
        }

        if (optionGroupBuilder != null) {
          optionGroupBuilder.consumeTag(in);
          if (optionGroupBuilder.isClosed()) {
            options.add(optionGroupBuilder.build(subCounts));
            optionGroupBuilder = null;
          }
          return;
        }

        switch (in.type()) {
          case OPTION_GROUP:
          case OPTION_GROUP_IF:
            closeUnit(in);
            optionGroupBuilder = new OptionGroup.Builder(in.cast());
            return;
          case OPTION:
          case OPTION_DEFAULT:
          case OPTION_IF:
          case OPTION_NONE:
            closeUnit(in);
            optionBuilder = new Option.Builder(in);
            return;
          default:
            break;
        }

        if (contentBuilder != null) contentBuilder.consumeTag(in);
        else if (optionBuilder != null) optionBuilder.consumeTag(in);
        else
          throw new CompilerException(in.tagNamePos(), "unexpected tag; expected another option");
      }

      @Override
      public void consumeText(Tokenizer.Text text) throws CompilerException {
        if (contentBuilder != null) contentBuilder.consumeText(text);
        else if (optionBuilder != null) optionBuilder.consumeText(text);
        else if (optionGroupBuilder != null) optionGroupBuilder.consumeText(text);
        else
          throw new CompilerException(
              text.startPos(),
              "found text in a [prompt], but without an [option] or [option_group]");
      }

      @Override
      public void consumeWhitespace(Tokenizer.Whitespace whitespace) throws CompilerException {
        if (contentBuilder != null) contentBuilder.consumeWhitespace(whitespace);
        else if (optionBuilder != null) optionBuilder.consumeWhitespace(whitespace);
        else if (optionGroupBuilder != null) optionGroupBuilder.consumeWhitespace(whitespace);
      }

      private void closeUnit(TypedTag next) throws CompilerException {
        if (contentBuilder != null) {
          content = contentBuilder.build();
          contentBuilder = null;
        } else if (optionBuilder != null) {
          if (next != null && optionBuilder.tag.type() == TypedTag.Type.OPTION_NONE)
            throw new CompilerException(
                next.tagNamePos(),
                "[option_none] must be the last option; additional options not allowed");

          options.add(optionBuilder.build(subCounts));
          optionBuilder = null;
        } else if (optionGroupBuilder != null) {
          options.add(optionGroupBuilder.build(subCounts));
          optionGroupBuilder = null;
        }
      }

      @Override
      public Prompt build() throws CompilerException {
        closeUnit(null);

        if (options.isEmpty())
          throw new CompilerException(tag.tagNamePos(), "[prompt] doesn't have any options");
        if (options.size() == 1)
          throw new CompilerException(tag.tagNamePos(), "[prompt] has only one option");
        if (subCounts.minimum == 0)
          throw new CompilerException(
              tag.tagNamePos(),
              "[prompt] might display zero options.  Did you forget [option_none]?");
        if (subCounts.maximum < 2)
          throw new CompilerException(
              tag.tagNamePos(), "[prompt] will only ever display exactly one option");

        return new Prompt(tag, content, options);
      }
    }
  }

  private final ImmutableList<TypedTag> atomicDefinitions;
  private final ImmutableList<StructDefinition> structDefinitions;
  private final ImmutableList<Block> blocks;
  private final ImmutableList<SubBlock> subBlocks;
  private final ImmutableList<Prompt> prompts;

  private AST(
      List<TypedTag> atomicDefinitions,
      List<StructDefinition> structDefinitions,
      List<Block> blocks,
      List<SubBlock> subBlocks,
      List<Prompt> prompts) {
    this.atomicDefinitions = ImmutableList.copyOf(atomicDefinitions);
    this.structDefinitions = ImmutableList.copyOf(structDefinitions);
    this.blocks = ImmutableList.copyOf(blocks);
    this.subBlocks = ImmutableList.copyOf(subBlocks);
    this.prompts = ImmutableList.copyOf(prompts);
  }

  @ASTChild
  @Override
  public ImmutableList<TypedTag> atomicDefinitions() {
    return atomicDefinitions;
  }

  @ASTChild
  @Override
  public ImmutableList<StructDefinition> structDefinitions() {
    return structDefinitions;
  }

  @ASTChild
  @Override
  public ImmutableList<Block> blocks() {
    return blocks;
  }

  @ASTChild
  @Override
  public ImmutableList<SubBlock> subBlocks() {
    return subBlocks;
  }

  @ASTChild
  @Override
  public ImmutableList<Prompt> prompts() {
    return prompts;
  }

  public static class Builder {
    // This format loses ordering of blocks and prompts but it can
    // be reconstructed from pos.
    private final List<TypedTag> atomicDefinitions = new ArrayList<>();
    private final List<StructDefinition> structDefinitions = new ArrayList<>();
    private final List<Block> blocks = new ArrayList<>();
    private final List<SubBlock> subBlocks = new ArrayList<>();
    private final List<Prompt> prompts = new ArrayList<>();

    TypedScopedBuilderWrapper<?> subBuilder = null;

    public Builder() {}

    public void consumeTag(TypedTag in) throws CompilerException {
      switch (in.type()) {
        case DEFINE:
        case DEFINE_ENUM:
        case EXTERN_BLOCK:
        case EXTERN_SCRIPT:
        case SCRIPT_TAG:
          closeUnit();
          atomicDefinitions.add(in);
          return;
        case DEFINE_STRUCT:
          closeUnit();
          subBuilder =
              TypedScopedBuilderWrapper.of(
                  new StructDefinition.Builder(in.cast()), structDefinitions);
          return;
        case BLOCK:
          closeUnit();
          subBuilder = TypedScopedBuilderWrapper.of(new Block.Builder(in.cast()), blocks);
          return;
        case SUB_BLOCK:
          closeUnit();
          subBuilder = TypedScopedBuilderWrapper.of(new SubBlock.Builder(in.cast()), subBlocks);
          return;
        case PROMPT:
          TypedTag.Prompt prompt = in.cast();
          if (prompt.hasId()) {
            closeUnit();
            subBuilder = TypedScopedBuilderWrapper.of(new Prompt.Builder(prompt), prompts);
            return;
          }
          break;
        default:
          break;
      }

      if (subBuilder != null) {
        subBuilder.consumeTag(in);
      } else {
        throw new CompilerException(
            in.tagNamePos(),
            String.format("[%s] tag cannot exist outside of a block or prompt", in.tagName()));
      }
    }

    public void consumeText(Tokenizer.Text text) throws CompilerException {
      if (subBuilder != null) {
        subBuilder.consumeText(text);
      } else {
        throw new CompilerException(
            text.startPos(), "text not associated with a [block] or [prompt]");
      }
    }

    public void consumeWhitespace(Tokenizer.Whitespace whitespace) throws CompilerException {
      if (subBuilder != null) {
        subBuilder.consumeWhitespace(whitespace);
      }
    }

    public AST build() throws CompilerException {
      closeUnit();
      return new AST(atomicDefinitions, structDefinitions, blocks, subBlocks, prompts);
    }

    private void closeUnit() throws CompilerException {
      if (subBuilder != null) {
        subBuilder.finish();
        subBuilder = null;
      }
    }
  }

  private static CompilerException unexpectedType(TypedTag tag) {
    return new CompilerException(tag.tagNamePos(), "unexpected tag");
  }
}
