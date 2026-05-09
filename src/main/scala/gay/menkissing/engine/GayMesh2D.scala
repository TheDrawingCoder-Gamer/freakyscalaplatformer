package gay.menkissing
package engine

import org.lwjgl.opengl.GL30.*
import org.lwjgl.opengl.GL11.*
import gay.menkissing.engine.graphics.Mesh2D
import gay.menkissing.engine.graphics.Color
import gay.menkissing.engine.graphics.GayMaterial

/**
  * 
  *
  * @param mesh
  * @param graphicalWidth
  * @param graphicalHeight
  */
class GayMesh2D(val mesh: Mesh2D, override var graphicalWidth: Int, override var graphicalHeight: Int) extends GayObject {
  var material: GayMaterial = GayMaterial.FlatColor(Color.fromHex(0xFFFFFFFF))

  def setColor(v: Color): Unit =
    material = GayMaterial.FlatColor(v)
  override def render(): Unit =
    if (!this.visible || !this.exists)
      return
    
    val cam = GayG.currentCamera
    val stack = cam.makeStack()
    cam.hotswapTo()

    val screenPos = getScreenPosition(cam)
    stack.translate(screenPos.x.toFloat, screenPos.y.toFloat, zIndex.toFloat)
    stack.scale(graphicalWidth, graphicalHeight, 1)
    material.bind(stack)
    mesh.draw()
    

  
}
