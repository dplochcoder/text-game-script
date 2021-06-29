package tgs;

import com.google.template.soy.SoyFileSet;
import com.google.template.soy.tofu.SoyTofu;

/**
 * Accessing this class loads the Soy Tofu libraries. This is relegated to its own class so that
 * loading of these libraries can be avoided if they're not needed.
 */
public class SublimeSyntaxTemplateHolder {
  private static final SoyTofu TOFU =
      SoyFileSet.builder()
          .add(SublimeSyntaxCompiler.class.getResource("TextGameScript.sublime-syntax.soy"))
          .build()
          .compileToTofu();

  public static SoyTofu tofu() {
    return TOFU;
  }
}
