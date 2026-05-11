package gay.menkissing.engine.graphics

import org.joml.Matrix4f
import org.lwjgl.opengl.*
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL13.*
import org.lwjgl.opengl.GL15.*
import org.lwjgl.opengl.GL20.*
import org.lwjgl.opengl.GL30.*
import org.lwjgl.stb.*
import org.lwjgl.stb.STBImage.*
import org.lwjgl.system.*
import org.lwjgl.system.MemoryStack.*
import org.lwjgl.system.MemoryUtil.*
import java.io.Closeable

import scala.util.Using

class Shader(vertex: Int, fragment: Int) extends Closeable {
  val program = glCreateProgram() 
  glAttachShader(program, vertex)
  glAttachShader(program, fragment)
  glLinkProgram(program)

  def use(): Unit =
    glUseProgram(program)

  def getLocation(name: String): Shader.UniformLocation =
    Shader.UniformLocation(glGetUniformLocation(program, name))


  def close(): Unit =
    glDeleteProgram(program)
}

object Shader {
  opaque type UniformLocation = Int
  object UniformLocation {
    def apply(v: Int): UniformLocation = v

    extension (self: UniformLocation) {
      def value: Int = self
      def bind(v: Float): Unit =
        glUniform1f(self, v)
      def bind(x: Float, y: Float): Unit =
        glUniform2f(self, x, y)
      def bind(x: Float, y: Float, z: Float): Unit =
        glUniform3f(self, x, y, z)
      def bind(x: Float, y: Float, z: Float, w: Float): Unit =
        glUniform4f(self, x, y, z, w)
      def bind(mtx: Matrix4f): Unit =
        Using.resource(stackPush()) { stack =>
          val buf = stack.mallocFloat(16)
          glUniformMatrix4fv(self, false, mtx.get(buf))
        }
      def bind(color: Color): Unit =
        color.bind(self)
    }
  }

  class SharedTextureShader(vertex: Int, fragment: Int) extends Shader(vertex, fragment) {
    val transformLoc = getLocation("transform")
    val texTransformLoc = getLocation("texTransform")

    def bindTransform(trans: Matrix4f, texTransform: Matrix4f): Unit =
      transformLoc.bind(trans)
      texTransformLoc.bind(texTransform)
  }

  class SolidColorShader(vertex: Int, fragment: Int) extends Shader(vertex, fragment) {
    val transformLoc = getLocation("transform")
    val colorLoc = getLocation("solidColor")

    def bindTransform(trans: Matrix4f): Unit =
      transformLoc.bind(trans)
  }

  private def makeShader(source: String, kind: Int): Int =
    val shader = glCreateShader(kind)
    glShaderSource(shader, source)
    glCompileShader(shader)
    shader
  
  private def vertexShader(source: String): Int =
    makeShader(source, GL_VERTEX_SHADER)
  
  private def fragmentShader(source: String): Int =
    makeShader(source, GL_FRAGMENT_SHADER)

  private val fontVertexShader = {
    val source = 
      """
      #version 330 core
      layout (location = 0) in vec4 vertex;
      out vec2 texCoord;

      uniform mat4 projection;

      void main()
      {
        gl_Position = projection * vec4(vertex.xy, 0.0, 1.0);
        texCoord = vertex.zw;
      }
      """
    val shader = glCreateShader(GL_VERTEX_SHADER)
    glShaderSource(shader, source)
    glCompileShader(shader)
    shader
  }
  private val textVertexShader = {
    val source =
      """#version 330 core
        |layout (location = 0) in vec4 vertex; // <vec2 pos, vec2 tex>
        |out vec2 texCoord;
        |
        |uniform mat4 projection;
        |
        |void main()
        |{
        |    gl_Position = projection * vec4(vertex.xy, 0.0, 1.0);
        |    texCoord = vertex.zw;
        |}""".stripMargin
    val shader = glCreateShader(GL_VERTEX_SHADER)
    glShaderSource(shader, source)
    glCompileShader(shader)
    shader
  }
  private val defaultVertexShader = {
    val freakyCode =
      """
      #version 330 core
      layout (location = 0) in vec2 pos;
      layout (location = 1) in vec2 texPos;

      out vec2 texCoord;

      uniform mat4 transform;
      uniform mat4 texTransform;

      void main()
      {
        gl_Position = transform * vec4(pos, 0.0f, 1.0f);
        texCoord = (texTransform * vec4(texPos, 0.0f, 1.0f)).xy;
      }
      """
    val shader = glCreateShader(GL_VERTEX_SHADER)
    glShaderSource(shader, freakyCode)
    glCompileShader(shader)
    shader
  }
  private val blendFragmentShader = {
    val freakyCode =
      """
      #version 330 core
      out vec4 FragColor;
      
      in vec2 texCoord;

      uniform sampler2D ourTexture;

      void main()
      {
        vec4 texel = texture(ourTexture, texCoord);
        FragColor = texel;
      }
      """
    val shader = glCreateShader(GL_VERTEX_SHADER)
    glShaderSource(shader, freakyCode)
    glCompileShader(shader)
    shader
  }
  private val cutoutFragmentShader = {
    val freakyCode =
      """
      #version 330 core
      out vec4 FragColor;
      
      in vec2 texCoord;

      uniform sampler2D ourTexture;

      void main()
      {
        vec4 texel = texture(ourTexture, texCoord);
        if (texel.a < 0.5) {
          discard;
        }
        FragColor = texel;
      }
      """
    val shader = glCreateShader(GL_FRAGMENT_SHADER)
    glShaderSource(shader, freakyCode)
    glCompileShader(shader)
    shader
  }
  private val solidColorFragmentShader = {
    val freakyCode =
      """
      #version 330 core
      out vec4 FragColor;

      in vec2 texCoord;

      uniform vec4 solidColor;

      void main()
      {
        FragColor = solidColor;
      }
      """
    val shader = glCreateShader(GL_FRAGMENT_SHADER)
    glShaderSource(shader, freakyCode)
    glCompileShader(shader)
    shader
  }
  private val textFragmentShader = {
    val source =
      """
        |#version 330 core
        |in vec2 texCoord;
        |out vec4 color;
        |
        |uniform sampler2D text;
        |uniform vec4 textColor;
        |
        |void main()
        |{
        |    vec4 sampled = vec4(1.0, 1.0, 1.0, texture(text, texCoord).r);
        |    color = textColor * sampled;
        |}
        |""".stripMargin
    val shader = glCreateShader(GL_FRAGMENT_SHADER)
    glShaderSource(shader, source)
    glCompileShader(shader)
    shader
  }
  private val msdfFragmentShader = {
    val source =
      """
      #version 330 core
      in vec2 texCoord;
      out vec4 color;

      uniform sampler2D text;
      uniform vec4 textColor;
      uniform vec2 aemRange;
      uniform float inverseWidth;
      uniform float thresholdEm;

      float median(float r, float g, float b) {
        return max(min(r, g), min(max(r, g), b));
      }

      void main() 
      {
        vec3 msd = texture(text, texCoord).rgb;
        float sd = median(msd.r, msd.g, msd.b);
        float distanceEm = mix(aemRange[1], aemRange[0], sd);
        float opacity = clamp((thresholdEm - distanceEm) * inverseWidth + 0.5, 0.0, 1.0);
        color = textColor * opacity;
      }
      """
    val shader = glCreateShader(GL_FRAGMENT_SHADER)
    glShaderSource(shader, source)
    glCompileShader(shader)
    shader
  }
  private val tsdfFragmentShader = {
    fragmentShader(
      """
      #version 330 core
      in vec2 texCoord;
      out vec4 color;

      uniform sampler2D text;
      uniform vec4 textColor;
      uniform vec2 aemRange;
      uniform float inverseWidth;
      uniform float thresholdEm;

      void main() {
        float sd = texture(text, texCoord).r;
        float distanceEm = mix(aemRange[1], aemRange[0], sd);
        float opacity = clamp((thresholdEm - distanceEm) * inverseWidth + 0.5, 0.0, 1.0);
        color = textColor * opacity;
      }
      """
    )
  }

  
  val cutoutProgram = SharedTextureShader(defaultVertexShader, cutoutFragmentShader)
  val blendProgram = SharedTextureShader(defaultVertexShader, blendFragmentShader)
  val solidColorProgram = 
    SolidColorShader(defaultVertexShader, solidColorFragmentShader)
  val textProgram = Shader(textVertexShader, textFragmentShader)
  val fontSoftMaskProgram = Shader(fontVertexShader, textFragmentShader)
  val fontMSDFProgram = Shader(fontVertexShader, msdfFragmentShader)
  val fontTSDFProgram = Shader(fontVertexShader, tsdfFragmentShader)
  
  glDeleteShader(defaultVertexShader)
  glDeleteShader(cutoutFragmentShader)
  glDeleteShader(blendFragmentShader)
  glDeleteShader(solidColorFragmentShader)
  glDeleteShader(textVertexShader)
  glDeleteShader(textFragmentShader)
  glDeleteShader(fontVertexShader)
  glDeleteShader(msdfFragmentShader)
}