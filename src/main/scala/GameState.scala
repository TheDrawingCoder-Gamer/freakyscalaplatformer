import scala.collection.{mutable => mut}
import gay.menkissing.common.math as gaymath
import scala.util.Using

import org.joml.Matrix3f
import org.lwjgl.opengl.*
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.system.*
import MemoryUtil.*
import MemoryStack.*

import GL11.*
import GL13.*
import GL15.*
import GL30.*
import GL20.*

trait GayBasic {
    def update(): Unit    
}

trait GayObject extends GayBasic {
    var x: Int = 0
    var xRemainder: Float = 0
    var y: Int = 0
    var yRemainder: Float = 0

    var hitX: Int = 0
    var hitY: Int = 0
    var hitW: Int = 8
    var hitH: Int = 8

    def moveRaw(bx: Float, by: Float): Unit = {
        xRemainder += bx
        val mx = (xRemainder + 0.5f).toInt
        xRemainder -= mx.toFloat
        x = x + mx

        yRemainder += by
        val my = (yRemainder + 0.5f).toInt
        yRemainder -= my.toFloat
        y = y + my
    }
}


sealed trait GayGraphic {
    def width: Int
    def height: Int

    def draw(matrices: _root_.draw.MatrixStack): Unit
}

case class GayTexture(tex: draw.Texture) extends GayGraphic {
    def width = tex.width
    def height = tex.height
    def draw(matrices: _root_.draw.MatrixStack): Unit = {
        tex.draw(matrices, 0, 0, tex.width, tex.height)
    }
}

class GayAtlas(val atlas: draw.TextureAtlas, var current: String) extends GayGraphic {
    def width = atlas.get(current).get.w
    def height = atlas.get(current).get.h
    def draw(matrices: _root_.draw.MatrixStack): Unit = {
        atlas.draw(matrices, current)
    }
}


trait GaySprite(var graphic: GayGraphic) extends GayObject, draw.Renderable {
    var scrollFactor: gaymath.PointF = gaymath.PointF(1, 1)
    def render(ctx: draw.GraphicsContext): Unit = ctx.stack.scoped {
        val matrices = ctx.stack
        matrices.translate(-(scrollFactor.x * ctx.camera.x.toFloat).toFloat, -(scrollFactor.y * ctx.camera.y.toFloat).toFloat, 0)
        matrices.translate(x.toFloat, y.toFloat, 0)
        matrices.scaleXY(graphic.width.toFloat, graphic.height.toFloat)
        
        graphic.draw(matrices)
    }

}

object Textures {
    val player = draw.Texture(getClass.getResourceAsStream("player.png"))
    val playerSheet = draw.Texture(getClass.getResourceAsStream("player_sheet.png"))
    val tiles = draw.Texture(getClass.getResourceAsStream("tiles.png"))
}
class TestObject(texture: draw.Texture) extends GaySprite(GayTexture(texture)) {
    var counter: Double = 0
    def update() = {
        counter = counter + 0.2
        x = (counter % renderWidth).toInt
    }
}

class FreakyObject(val input: Input, atlas: draw.TextureAtlas) extends GaySprite(GayAtlas(atlas, "0")) {
    var counter: Int = 0
    def update() = {
        counter += 1
        val index = ((counter / 10) % 2).toString
        graphic.asInstanceOf[GayAtlas].current = index

        moveRaw(input.inputX.toFloat, input.inputY.toFloat)
    }
}



class GameState() {
    Textures
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    val objects = mut.ListBuffer[GayBasic]()
    var camera = gaymath.Point(0, 0)
    val input1 = Input(0)
    val world = World.load()
    objects.append(new FreakyObject(input1,draw.TextureAtlas.split(Textures.playerSheet, 4, 1)))
    def run(): Unit = {
        val stack = draw.MatrixStack()
        stack.scale(1, -1, 1)
        stack.ortho(0, renderWidth, 0, renderHeight, 0, 1)
        input1.update()
        drawMap(stack)
        for (obj <- objects) {
            obj.update()
            obj match {
                case o: draw.Renderable => o.render(draw.GraphicsContext(stack, camera))
                case _ => ()
            }
        }
    }
    def drawMap(stack: draw.MatrixStack): Unit = stack.scoped {
        stack.translate(-camera.x, -camera.y, 0)
        val ccx = math.floorDiv(camera.x, 8)
        val ccy = math.floorDiv(camera.y, 8)

        val xOff = -(camera.x % 8)
        val yOff = -(camera.y % 8)

        val minx = ccx
        val miny = ccy

        val maxx = ccx + 42
        val maxy = ccy + 25

        for (i <- minx until maxx) {
            for (j <- miny until maxy) {
                val bgTile = world.tiles.bg.get(i, j)
                val fgTile = world.tiles.fg.get(i, j)
                stack.scoped {
                    stack.translate((i - minx) * 8, (j - miny) * 8, 0)
                    stack.scaleXY(8, 8)
                    bgTile.foreach { tile =>
                        Textures.tiles.draw(stack, (tile % 16) * 8, math.floorDiv(tile, 16) * 8, 8, 8)
                    }
                    fgTile.foreach { tile =>
                        Textures.tiles.draw(stack, (tile % 16) * 8, math.floorDiv(tile, 16) * 8, 8, 8)
                    }
                }
            }
        }
    }
}
