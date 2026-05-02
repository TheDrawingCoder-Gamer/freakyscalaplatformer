package gay.menkissing.engine

import gay.menkissing.{GameState, Player}
import gay.menkissing.common.math as gaymath
import gay.menkissing.engine.GayView
import org.joml.Matrix4f
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.opengl.*
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL13.*
import org.lwjgl.opengl.GL15.*
import org.lwjgl.opengl.GL20.*
import org.lwjgl.opengl.GL30.*
import org.lwjgl.system.*
import org.lwjgl.system.MemoryStack.*
import org.lwjgl.system.MemoryUtil.*

import scala.collection.mutable
import scala.util.Using
import gay.menkissing.draw

open class GayBasic extends draw.Renderable {
    var visible: Boolean = true
    var active: Boolean = true
    var exists: Boolean = true

    private val _cameras = SpriteCameras()
    def cameras: SpriteCameras = _cameras

    var destroyed: Boolean = false

    protected var _container: Option[GayContainer] = None
    def container: Option[GayContainer] = _container
    def container_=(v: Option[GayContainer]): Unit = _container = v

    def update(): Unit = ()
    def render(): Unit = ()

    def destroy(): Unit = {
        // freaky!
        destroyed = true
    }
}


open class GayObject extends GayBasic {
    
    protected var _x: Int = 0
    def x: Int = _x
    def x_=(v: Int): Unit = _x = v
    
    var xRemainder: Float = 0
    protected var _y: Int = 0
    def y: Int = _y
    def y_=(v: Int): Unit = _y = v
    
    var yRemainder: Float = 0
    var zIndex: Int = 0

    var hitX: Int = 0
    var hitY: Int = 0
    var hitW: Int = 8
    var hitH: Int = 8



    var solid: Boolean = false

    var facingRight: Boolean = true

    var speedX: Float = 0
    var speedY: Float = 0

    var scrollFactor: gaymath.PointF = gaymath.PointF(1, 1)

    def getScreenPosition(camera: GayView): gaymath.PointF =
        gaymath.PointF(x - (camera.viewPos.x * scrollFactor.x), y - (camera.viewPos.y * scrollFactor.y))

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

        Game.instance.gamemanager.currentState.exists {
            case it: GayObject =>
                it.solid && it != this && !it.destroyed && it.testOverlaps(hitbox)
            case _ => false
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
    

    def viewableOnCamera(cam: GayView): Boolean =
        visible && ((if (!cameras.removeFromDefault) Game.instance.gamemanager.cameras.defaults.contains(cam) else false) || cameras.cameras(cam))

    def touch(player: Player): Unit = ()
    def canTouch(player: Player): Boolean = false

    def testOverlaps(thatHitbox: gaymath.Rect): Boolean =
        worldHitbox.overlaps(thatHitbox)
    
    def squish(): Unit = die()
}



def solidMap(x: Int, y: Int): Boolean = {
    Game.instance.gamemanager.currentState.asInstanceOf[GameState].world.tiles.fg.get(x, y).isDefined
}

sealed trait GayGraphic {
    def width: Int
    def height: Int

    def render(matrices: Matrix4f): Unit
}

final class GayRect(var width: Int, var height: Int, var color: draw.Color) extends GayGraphic {
    override def render(matrices: Matrix4f): Unit =
        draw.setSolidColor(color, matrices)
        draw.filledRect()
}

final case class GayTexture(tex: draw.Texture) extends GayGraphic {
    def width = tex.width
    def height = tex.height
    def render(matrices: Matrix4f): Unit = {
        tex.draw(matrices, 0, 0, tex.width, tex.height)
    }
}

class GayAtlas(val atlas: draw.TextureAtlas, var current: String) extends GayGraphic {
    def width = atlas(current).w
    def height = atlas(current).h
    def render(matrices: Matrix4f): Unit = {
        atlas.draw(matrices, current)
    }
}


class GaySprite(var graphic: GayGraphic) extends GayObject {
    override def render(): Unit =  {
        Game.instance.gamemanager.cameras.camList.withFilter(this.viewableOnCamera).foreach { cam =>
          val stack = cam.makeStack()
          cam.hotswapTo()

          val screenPos = getScreenPosition(cam)
          stack.translate(screenPos.x.toFloat, screenPos.y.toFloat, zIndex.toFloat)

          if (!facingRight) {
              stack.translate(graphic.width.toFloat, 0, 0)
              stack.scale(-1, 1, 1)
          }

          stack.scale(graphic.width, graphic.height, 1)

          graphic.render(stack)
        }
    }

}

class TestObject(texture: draw.Texture) extends GaySprite(GayTexture(texture)) {
    var counter: Double = 0
    override def update() = {
        counter = counter + 0.2
        x = (counter % draw.renderWidth).toInt
    }
}

class FreakyObject(val input: Input, atlas: draw.TextureAtlas) extends GaySprite(GayAtlas(atlas, "0")) {
    var counter: Int = 0
    override def update() = {
        counter += 1
        val index = ((counter / 10) % 2).toString
        graphic.asInstanceOf[GayAtlas].current = index

        moveRaw(input.inputX.toFloat, input.inputY.toFloat)
    }
}

class GayText(var text: String, var textHeight: Int, var color: draw.Color) extends GayObject {
    override def render(): Unit = {
        Game.instance.gamemanager.cameras.camList.withFilter(this.viewableOnCamera).foreach { cam =>
            val matrices = cam.makeStack()
            cam.hotswapTo()

            matrices.translate(x.toFloat, y.toFloat, zIndex.toFloat)
            val ratio = GayText.ratioFor(textHeight)
            matrices.scale(ratio, ratio, 1f)

            draw.drawText(text, color, matrices)
        }

    }
}

object GayText {
    def ratioFor(height: Int): Float =
        height.toFloat / draw.TextRendering.textSize.toFloat
}

abstract class GayState(val manager: GameManager) extends GayTypedContainer[GayBasic] {
    def start(): Unit


    def close(): Unit

    def frame: Int = manager.frame
}


