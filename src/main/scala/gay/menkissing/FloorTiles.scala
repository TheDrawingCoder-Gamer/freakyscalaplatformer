package gay.menkissing

import gay.menkissing.common.math.Rect

// Tilemap that is considered solid when its _not_ defined
class FloorTiles(tileMapTex: draw.Texture, tileSize: Int, tileMap: TileMap) 
  extends engine.GayTiles(tileMapTex, tileSize, tileMap) {
  
  
  override def testOverlaps(thatHitbox: Rect): Boolean =
    val imin = math.floorDiv(thatHitbox.x, tileSize)
    val imax = math.floorDiv(thatHitbox.right - 1, tileSize)
    val jmin = math.floorDiv(thatHitbox.y, tileSize)
    val jmax = math.floorDiv(thatHitbox.bottom - 1, tileSize)

    (imin to imax).exists { i =>
      (jmin to jmax).exists { j =>
        tileMap.get(i, j).isEmpty
      }
    }
}
