package gay.menkissing
package engine
import gay.menkissing.engine.graphics.Color
import gay.menkissing.engine.graphics.GayMaterial

class GayRectSprite(width: Int, height: Int, color: Color) extends GayMesh2D(draw.squareVertices, width, height) {
  material = GayMaterial.FlatColor(color)
}
