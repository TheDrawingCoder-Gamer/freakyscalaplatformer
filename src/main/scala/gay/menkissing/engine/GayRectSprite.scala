package gay.menkissing
package engine
import gay.menkissing.engine.graphics.Color
import gay.menkissing.engine.graphics.GayMaterial
import gay.menkissing.engine.graphics.Mesh2D

class GayRectSprite(width: Int, height: Int, color: Color) extends GayMesh2D(Mesh2D.squareMesh, width, height) {
  material = GayMaterial.FlatColor(color)
}
