package gay.menkissing.engine

import gay.menkissing.common.math as gaymath
import gay.menkissing.common.math.Rect
import gay.menkissing.TileMap
import gay.menkissing.engine.graphics.MatrixStack
import gay.menkissing.engine.graphics.Texture
import gay.menkissing.engine.graphics.Mesh2D

class GayTiles(val tileMapTex: Texture, val tileSize: Int, val tileMap: TileMap) extends GayObject {
  val tileMapWidth: Int = tileMapTex.width / tileSize
  val tileMapHeight: Int = tileMapTex.height / tileSize

  var enableAutoDepth: Boolean = false

  if (tileMapTex.width % tileSize != 0 || tileMapTex.height % tileSize != 0)
    throw RuntimeException("Tile texture size not multiple of tile size")

  override def render(): Unit =
    val cam = GayG.currentCamera
    val matrices = cam.makeStack()
    tileMapTex.bind()
    Mesh2D.squareMesh.bind()

    // ??? TODO: how to avoid rendering everything
    for {
      i <- 0 until tileMap.width
      j <- 0 until tileMap.height
    } {
      val tile = tileMap.get(i, j)
      tile.foreach { t =>
        matrices.scoped {
          val x = i * tileSize
          val y = j * tileSize
          val screenPos =
            gaymath.PointF(x - (cam.viewPos.x * scrollFactor.x), y - (cam.viewPos.y * scrollFactor.y))

          matrices.translate(screenPos.x.toFloat, screenPos.y.toFloat, zIndex)
          if (enableAutoDepth) {
            matrices.translate(0, 0, j * tileSize + (tileSize / 2))
          }
          matrices.scaleXY(tileSize, tileSize)
          tileMapTex.renderMode.program.bindTransform(matrices, tileMapTex.texTransform((t % tileMapWidth) * tileSize, math.floorDiv(t, tileMapWidth) * tileSize, tileSize, tileSize))
          Mesh2D.squareMesh.unsafeDraw()
          
        }
      }
      
    }

    
    

  override def testOverlaps(thatHitbox: Rect): Boolean =
    val imin = math.floorDiv(thatHitbox.x, tileSize)
    val imax = math.floorDiv(thatHitbox.right - 1, tileSize)
    val jmin = math.floorDiv(thatHitbox.y, tileSize)
    val jmax = math.floorDiv(thatHitbox.bottom - 1, tileSize)

    (imin to imax).exists { i =>
      (jmin to jmax).exists { j =>
        tileMap.get(i, j).isDefined
      }
    }
}
