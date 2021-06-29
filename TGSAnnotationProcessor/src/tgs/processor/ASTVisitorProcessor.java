package tgs.processor;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Writer;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.Processor;
import javax.annotation.processing.RoundEnvironment;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.TypeElement;
import javax.tools.Diagnostic.Kind;
import javax.tools.FileObject;
import javax.tools.JavaFileObject;
import javax.tools.StandardLocation;

import com.google.auto.service.AutoService;
import com.google.common.collect.ImmutableSet;
import com.squareup.javapoet.ClassName;
import com.squareup.javapoet.JavaFile;
import com.squareup.javapoet.MethodSpec;
import com.squareup.javapoet.ParameterSpec;
import com.squareup.javapoet.ParameterizedTypeName;
import com.squareup.javapoet.TypeName;
import com.squareup.javapoet.TypeSpec;
import com.squareup.javapoet.TypeVariableName;

@AutoService(Processor.class)
public class ASTVisitorProcessor extends AbstractProcessor {

  @Override
  public SourceVersion getSupportedSourceVersion() {
    return SourceVersion.latestSupported();
  }

  @Override
  public ImmutableSet<String> getSupportedAnnotationTypes() {
    return ImmutableSet.of(ASTNode.class.getName(), ASTChild.class.getName());
  }

  private Set<String> allAstNodes = new HashSet<>();

  @Override
  public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
    try {
      if (roundEnv.processingOver()) {
        generateASTVisitorFiles();
      } else if (!annotations.isEmpty()) {
        processImpl(roundEnv);
      }
    } catch (IOException ex) {
      throw new RuntimeException(ex);
    }

    return true;
  }

  private void getAllAstNodes() throws IOException {
    boolean needsCreate = true;
    FileObject file = null;
    try {
      file =
          processingEnv
              .getFiler()
              .getResource(StandardLocation.CLASS_OUTPUT, "", "META-INF/astNodes/list.txt");
      try (BufferedReader br = new BufferedReader(new InputStreamReader(file.openInputStream()))) {
        String line;
        while ((line = br.readLine()) != null) {
          line = line.trim();
          if (!line.isEmpty()) {
            allAstNodes.add(line);
          }
        }
      }

      needsCreate = false;
    } catch (IOException ignore) {
    }

    if (needsCreate) {
      file =
          processingEnv
              .getFiler()
              .createResource(StandardLocation.CLASS_OUTPUT, "", "META-INF/astNodes/list.txt");
    }
    try (Writer wr = file.openWriter()) {
      wr.append(allAstNodes.stream().sorted().collect(Collectors.joining("\n", "", "\n")));
    }
  }

  @FunctionalInterface
  private static interface TypeRenderer {
    String renderType(String typeName);
  }

  private void writeFile(String name, String format, TypeRenderer typeRenderer) throws IOException {
    JavaFileObject file = processingEnv.getFiler().createSourceFile("tgs." + name);
    try (Writer wr = file.openWriter()) {
      wr.append(
          String.format(
              format,
              allAstNodes
                  .stream()
                  .sorted()
                  .map(typeRenderer::renderType)
                  .collect(Collectors.joining("\n\n"))));
    }
  }

  private void generateASTVisitorFiles() throws IOException {
    getAllAstNodes();

    writeFile(
        "ASTVisitor",
        "package tgs;\n\ninterface ASTVisitor<V>{\n\n%s\n\n}\n",
        typeName -> String.format("  V visit(%s node, V value);", typeName));
    writeFile(
        "DefaultASTVisitor",
        "package tgs;\n\n"
            + "public abstract class DefaultASTVisitor<V> implements ASTVisitor<V> {\n\n"
            + "%s\n\n}\n",
        typeName ->
            String.format(
                "  @Override\n"
                    + "  public V visit(%s node, V value) {\n"
                    + "    return node.visitChildren(this, value);\n"
                    + "  }",
                typeName));
    writeFile(
        "VoidDefaultASTVisitor",
        "package tgs;\n\n"
            + "public abstract class VoidDefaultASTVisitor extends DefaultASTVisitor<Void> {\n\n"
            + "%s\n\n}\n",
        typeName ->
            String.format(
                "  @Override\n"
                    + "  public final Void visit(%s node, Void value) {\n"
                    + "    visitImpl(node);\n"
                    + "    return null;\n"
                    + "  }\n\n"
                    + "  public void visitImpl(%s node) {\n"
                    + "    node.visitChildren(this, null);\n"
                    + "  }",
                typeName, typeName));
  }

  private static String getASTNodeClassName(Element element) {
    Deque<String> elems = new ArrayDeque<>();
    elems.push("ASTNode");
    do {
      if (element.getKind() == ElementKind.CLASS) {
        elems.addFirst(element.getSimpleName().toString());
      }
      element = element.getEnclosingElement();
    } while (element.getKind() != ElementKind.PACKAGE);
    return elems.stream().collect(Collectors.joining("_"));
  }

  private static final ClassName AST_NODE_INTERFACE_NAME = ClassName.get("tgs", "ASTNodeInterface");
  private static final ClassName AST_VISITOR_NAME = ClassName.get("tgs", "ASTVisitor");
  private static final TypeVariableName V = TypeVariableName.get("V");
  private static final ClassName AST_NODE_UTILS_NAME = ClassName.get("tgs", "ASTNodeUtils");

  private void writeASTNodeFile(TypeElement element) throws IOException {
    String getInterfaceName = getASTNodeClassName(element);
    if (!element
        .getInterfaces()
        .stream()
        .anyMatch(i -> TypeName.get(i).toString().endsWith(getInterfaceName))) {
      processingEnv
          .getMessager()
          .printMessage(Kind.ERROR, "Missing interface: " + getInterfaceName, element);
      return;
    }

    TypeSpec.Builder typeSpecBuilder =
        TypeSpec.interfaceBuilder(getInterfaceName)
            .addModifiers(Modifier.PUBLIC)
            .addSuperinterface(AST_NODE_INTERFACE_NAME);

    typeSpecBuilder.addMethod(
        MethodSpec.methodBuilder("accept")
            .addAnnotation(Override.class)
            .addModifiers(Modifier.PUBLIC, Modifier.DEFAULT)
            .addTypeVariable(V)
            .returns(V)
            .addParameter(
                ParameterSpec.builder(ParameterizedTypeName.get(AST_VISITOR_NAME, V), "visitor")
                    .build())
            .addParameter(ParameterSpec.builder(V, "value").build())
            .addStatement(
                "return visitor.visit(($L) this, value)", element.getQualifiedName().toString())
            .build());

    MethodSpec.Builder visitChildrenMethodBuilder =
        MethodSpec.methodBuilder("visitChildren")
            .addAnnotation(Override.class)
            .addModifiers(Modifier.PUBLIC, Modifier.DEFAULT)
            .addTypeVariable(V)
            .returns(V)
            .addParameter(
                ParameterSpec.builder(ParameterizedTypeName.get(AST_VISITOR_NAME, V), "visitor")
                    .build())
            .addParameter(ParameterSpec.builder(V, "value").build());
    for (Element maybeMethod : element.getEnclosedElements()) {
      if (maybeMethod.getKind() != ElementKind.METHOD) continue;
      if (maybeMethod.getAnnotation(ASTChild.class) == null) continue;
      if (maybeMethod.getAnnotation(Override.class) == null) {
        processingEnv.getMessager().printMessage(Kind.ERROR, "Missing @Override", maybeMethod);
      }

      ExecutableElement method = (ExecutableElement) maybeMethod;
      typeSpecBuilder.addMethod(
          MethodSpec.methodBuilder(method.getSimpleName().toString())
              .addModifiers(Modifier.PUBLIC, Modifier.ABSTRACT)
              .returns(TypeName.get(method.getReturnType()))
              .build());

      visitChildrenMethodBuilder.addStatement(
          "value = $T.accept($L(), visitor, value)",
          AST_NODE_UTILS_NAME,
          method.getSimpleName().toString());
    }
    typeSpecBuilder.addMethod(visitChildrenMethodBuilder.addStatement("return value").build());

    JavaFile javaFile = JavaFile.builder("tgs", typeSpecBuilder.build()).build();
    JavaFileObject file = processingEnv.getFiler().createSourceFile("tgs." + getInterfaceName);
    try (Writer wr = file.openWriter()) {
      wr.append(javaFile.toString());
    }
  }

  private void processImpl(RoundEnvironment roundEnv) throws IOException {
    for (Element element : roundEnv.getElementsAnnotatedWith(ASTNode.class)) {
      TypeElement typeElement = (TypeElement) element;
      try {
        writeASTNodeFile(typeElement);
      } catch (Exception ex) {
        processingEnv.getMessager().printMessage(Kind.ERROR, "APT Error: " + ex, typeElement);
      }

      String extra = "";
      if (typeElement.getTypeParameters().size() > 0) {
        extra =
            typeElement
                .getTypeParameters()
                .stream()
                .map(p -> "?")
                .collect(Collectors.joining(", ", "<", ">"));
      }
      allAstNodes.add(typeElement.getQualifiedName().toString() + extra);
    }
  }
}
