package gay.menkissing
package engine

import gay.menkissing.draw.Vertices
import org.lwjgl.opengl.GL30.*
import org.lwjgl.opengl.GL11.*

class GayMesh2D(val mesh: Vertices, override var graphicalWidth: Int, override var graphicalHeight: Int) extends GayObject {
  var color: draw.Color = draw.Color.fromHex(0xFFFFFFFF)
  override def render(): Unit =
    if (!this.visible || !this.exists)
      return
    
    val cam = this.layer
    val stack = cam.makeStack()
    cam.hotswapTo()

    val screenPos = getScreenPosition(cam)
    stack.translate(screenPos.x.toFloat, screenPos.y.toFloat, zIndex.toFloat)
    stack.scale(graphicalWidth, graphicalHeight, 1)
    draw.setSolidColor(color, stack)
    glBindVertexArray(mesh.VAO)
    glDrawElements(GL_TRIANGLES, 3, GL_UNSIGNED_INT, 0)
    

  
}
