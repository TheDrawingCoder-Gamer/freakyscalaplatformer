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
import gay.menkissing.engine.group.{GayContainer, GayTypedContainer}

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
    
    def graphicalWidth: Int = 0
    def graphicalWidth_=(v: Int): Unit = ()
    def graphicalHeight: Int = 0
    def graphicalHeight_=(v: Int): Unit = ()

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