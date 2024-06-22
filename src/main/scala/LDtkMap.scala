import upickle.default.*
import collection.mutable as mut
import gay.menkissing.common.math as gaymath
import java.io.InputStreamReader

case class LDtkRawFieldInstance(__identifier: String, __value: ujson.Value, __type: String)
    derives ReadWriter

case class LDtkEntityInstance(__identifier: String, __grid: (Int, Int), fieldInstances: List[LDtkRawFieldInstance], 
    __worldX: Int, __worldY: Int, width: Int, height: Int)
    derives ReadWriter

case class LDtkAutoLayerTile(px: (Int, Int), t: Int)
    derives ReadWriter

case class LDtkLayerInstance(__type: String, __identifier: String, entityInstances: List[LDtkEntityInstance],
    autoLayerTiles: List[LDtkAutoLayerTile])
    derives ReadWriter

case class LDtkLevel(worldX: Int, worldY: Int, pxWid: Int, pxHei: Int, 
    layerInstances: List[LDtkLayerInstance], fieldInstances: List[LDtkRawFieldInstance])
    derives ReadWriter

case class LDtkMap(levels: List[LDtkLevel])
    derives ReadWriter

case class TileMap(width: Int, height: Int, items: collection.mutable.ArrayBuffer[Int]) {
    def get(x: Int, y: Int): Option[Int] = {
        if (x < 0 || x >= width)
            return None
        if (y < 0 || y >= height)
            return None
        val res = items(y * width + x)
        
        if (res < 0)
            None
        else
            Some(res)
    }
}



object TileMap {

    def fromMap(map: collection.AbstractMap[(Int, Int), Int]): TileMap = {
        val width = map.keys.maxBy(_._1)._1 + 1
        val height = map.keys.maxBy(_._2)._2 + 1

        val buf = mut.ArrayBuffer.fill[Int](width * height)(0)

        for (y <- 0 until height) {
            for (x <- 0 until width) {
                buf(y * width + x) = map.getOrElse((x, y), -1)
            }
        }

        TileMap(width, height, buf)
    }
}

case class Tiles(fg: TileMap, bg: TileMap)

enum CamMode {
    case Locked, FollowX, FollowY, FreeFollow
}

object CamMode {
    def fromString(str: String): CamMode = str match {
        case "Locked" => CamMode.Locked
        case "FollowX" => CamMode.FollowX
        case "FollowY" => CamMode.FollowY
        case "FreeFollow" => CamMode.FreeFollow
        case _ => throw new RuntimeException("not a good cammode")
    }
}

enum EntityData {
    case PlayerStart(worldStart: Boolean)
}

case class Entity(x: Int, y: Int, w: Int, h: Int, data: EntityData)

object Entity {
    def fromLDtk(x: Int, y: Int, entity: LDtkEntityInstance): Option[Entity] = {
        val ex = entity.__worldX
        val ey = entity.__worldY
        val ew = entity.width
        val eh = entity.height
        
        val entityData = entity.__identifier match {
            case "PlayerStart" => {
                var worldStart = false
                for (field <- entity.fieldInstances) {
                    if (field.__identifier == "WorldStart") {
                        worldStart = field.__value.bool
                    }
                }
                Some(EntityData.PlayerStart(worldStart))
            }
            case _ => None
        }

        
        entityData.map(it => Entity(ex, ey, ew, eh, it))
    }
}


case class Level(x: Int, y: Int, width: Int, height: Int, deathBottom: Boolean, camMode: CamMode, entities: List[Entity]) 

def addTilesToMap(tileX: Int, tileY: Int, map: mut.HashMap[(Int, Int), Int], tiles: List[LDtkAutoLayerTile]): Unit = {
    for (autoTile <- tiles) {
        val tx = math.floorDiv(autoTile.px._1, 8)
        val ty = math.floorDiv(autoTile.px._2, 8)

        map((tileX + tx, tileY + ty)) = autoTile.t
    }
}

case class World(levels: List[Level], tiles: Tiles)

object World {
    def fromLDtk(map: LDtkMap): World = {
        val bgMap = mut.HashMap[(Int, Int), Int]()
        val fgMap = mut.HashMap[(Int, Int), Int]()
        val levels = mut.ListBuffer[Level]()
        for (level <- map.levels) {
            val tileX = math.floorDiv(level.worldX, 8)
            val tileY = math.floorDiv(level.worldY, 8)
            val tileW = math.floorDiv(level.pxWid, 8)
            val tileH = math.floorDiv(level.pxHei, 8)
            val entities = mut.ListBuffer[Entity]()
            var bottomDeath = false
            var camMode = CamMode.Locked

            for (layer <- level.layerInstances) {
                layer.__identifier match {
                    case "Fg" => addTilesToMap(tileX, tileY, fgMap, layer.autoLayerTiles)
                    case "Bg" => addTilesToMap(tileX, tileY, bgMap, layer.autoLayerTiles)
                    case "Entities" => {
                        for (entity <- layer.entityInstances) {
                            Entity.fromLDtk(level.worldX, level.worldY, entity).foreach(entities.append)
                        }
                    }
                }
            }
            for (field <- level.fieldInstances) {
                field.__identifier match {
                    case "CamMode" =>
                        camMode = CamMode.fromString(field.__value.str)
                    case "BottomDeath" =>
                        bottomDeath = field.__value.bool
                    case _ => ()
                } 
            }

            levels.append(Level(tileX, tileY, tileW, tileH, bottomDeath, camMode, entities.toList))
        }
        val tiles = Tiles(TileMap.fromMap(fgMap), TileMap.fromMap(bgMap))

        World(levels.toList, tiles)
    }
    def load(): World = {
        val map = ujson.InputStreamParser.transform[LDtkMap](getClass.getResourceAsStream("map.ldtk"), upickle.default.reader[LDtkMap])
        fromLDtk(map)
    }
}
