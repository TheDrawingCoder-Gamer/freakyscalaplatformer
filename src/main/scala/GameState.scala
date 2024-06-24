import scala.collection.{mutable => mut}
import gay.menkissing.common.math as gaymath
import scala.util.Using

import org.joml.Matrix4f
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

    var solid: Boolean = false
    var destroyed: Boolean = false

    var facingRight: Boolean = true

    var speedX: Float = 0
    var speedY: Float = 0

    def worldHitbox: gaymath.Rect = {
        gaymath.Rect(x + hitX, y + hitY, hitW, hitH)
    }
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
    def checkSolid(ox: Int, oy: Int): Boolean = {
        val hitbox = worldHitbox.offset(ox, oy)
        val imin = math.floorDiv(hitbox.x, 8)
        val imax = math.floorDiv(hitbox.right - 1, 8)
        val jmin = math.floorDiv(hitbox.y, 8)
        val jmax = math.floorDiv(hitbox.bottom - 1, 8)
        val res = (imin to imax).exists { i =>
            (jmin to jmax).exists { j => 
                solidMap(i, j)
            }
        }
        if (res)
            return true

        return game.gamestate.objects.exists { it =>
            it match {
                case o: GayObject => 
                    o.solid && o != this && !o.destroyed && hitbox.overlaps(o.worldHitbox)
                case _ => false
            }
        }
    }
   
    def moveX(bx: Float, onCollide: Option[(Int, Int) => Boolean]): Boolean = {
        xRemainder += bx
        var mx: Int = (xRemainder + 0.5).toInt
        xRemainder -= mx.toFloat

        val total = mx
        val mxs = math.signum(mx)

        while (mx != 0) {
            if (checkSolid(mxs, 0)) {
                return onCollide match {
                    case Some(func) => func(total - mx, total)
                    case None => true
                }
            } else {
                x = x + mxs
                mx -= mxs
            }
        }

        false
    }
    def moveY(by: Float, onCollide: Option[(Int, Int) => Boolean]): Boolean = {
        yRemainder += by
        var my: Int = (yRemainder + 0.5).toInt
        yRemainder -= my.toFloat

        val total = my
        val mys = math.signum(my)

        while (my != 0) {
            if (checkSolid(0, mys)) {
                return onCollide match {
                    case Some(func) => func(total - my, total)
                    case None => true
                }
            } else {
                y = y + mys
                my -= mys
            }
        }

        false
    }
    def onCollideX(moved: Int, target: Int): Boolean = {
        xRemainder = 0
        speedX = 0
        true
    }
    def onCollideY(moved: Int, target: Int): Boolean = {
        yRemainder = 0
        speedY = 0
        true
    }
    def die(): Unit = ()
}


def solidMap(x: Int, y: Int): Boolean = {
    game.gamestate.world.tiles.fg.get(x, y).isDefined
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
        if (!facingRight) {
            matrices.translate(graphic.width.toFloat, 0, 0)
            matrices.scale(-1, 1, 1)
        }   
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
    val framebufferTex = glGenTextures()
    var frame = 0
    glBindTexture(GL_TEXTURE_2D, framebufferTex)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP)
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, draw.renderWidth, draw.renderHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, 0)
    glBindTexture(GL_TEXTURE_2D, 0)
    val rbo = glGenRenderbuffers()
    glBindRenderbuffer(GL_RENDERBUFFER, rbo)
    glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT, draw.renderWidth, draw.renderHeight)
    glBindRenderbuffer(GL_RENDERBUFFER, 0)
    val framebuffer = glGenFramebuffers()
    glBindFramebuffer(GL_FRAMEBUFFER, framebuffer)
    glFramebufferTexture2D(GL_FRAMEBUFFER,
                        GL_COLOR_ATTACHMENT0,
                        GL_TEXTURE_2D,
                        framebufferTex,
                        0)
    glFramebufferRenderbuffer(GL_FRAMEBUFFER,
                        GL_DEPTH_ATTACHMENT,
                        GL_RENDERBUFFER,
                        rbo)
    glBindFramebuffer(GL_FRAMEBUFFER, 0)
    val screenTransform = Matrix4f()
    objects.append(new Player(input1))
    def run(): Unit = {
        glBindFramebuffer(GL_FRAMEBUFFER, framebuffer)
        glViewport(0, 0, renderWidth, renderHeight)
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
        val stack = draw.MatrixStack()
        
        stack.ortho(0, renderWidth, renderHeight, 0, -1, 1)
        input1.update()
        drawMap(stack)
        for (obj <- objects) {
            obj.update()
            obj match {
                case o: draw.Renderable => o.render(draw.GraphicsContext(stack, camera))
                case _ => ()
            }
        }
        glBindFramebuffer(GL_FRAMEBUFFER, 0)
        glViewport(0, 0, windowWidth, windowHeight)

        glUseProgram(draw.Shaders.defaultProgram)
        glBindVertexArray(draw.screenVertices.VAO)
        glActiveTexture(GL_TEXTURE0)
        glBindTexture(GL_TEXTURE_2D, framebufferTex)


        draw.bindTransform(draw.Shaders.defaultProgram, screenTransform, Matrix4f())

        glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0)
        frame += 1
        frame %= 32767

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
