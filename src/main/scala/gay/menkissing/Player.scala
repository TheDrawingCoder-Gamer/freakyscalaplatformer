package gay.menkissing

import gay.menkissing.common.math as gaymath
import gay.menkissing.engine.{Game, GayAtlas, GayObject, GaySprite, Input}
import org.joml.Matrix4f
import org.lwjgl.opengl.*
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL13.*
import org.lwjgl.opengl.GL15.*
import org.lwjgl.opengl.GL20.*
import org.lwjgl.opengl.GL30.*

object Player {
    enum State {
        case Normal
    }
    val maxFallSpeed = 2.8f
    val maxFastFallSpeed = 3.4f
    
    val walkSpeed = 1.5f


    val playerAtlas = engine.graphics.TextureAtlas.splitBySize(Textures.preload.playerSheet, 16, 16)

}

class Player(val input: Input) extends GaySprite(GayAtlas(Player.playerAtlas, "1")) {
    var state: Player.State = Player.State.Normal 
    var tJumpGrace: Int = 0
    var jumpGraceY: Int = 0
    var crouching: Boolean = false
    var tVarJump: Int = 0
    var autoVarJump: Boolean = false
    var varJumpSpeed: Float = 0
    var spr: Int = 0

    var lastDir: Int = 0


    var tDeath: Int = 0

    hitX = 4
    hitY = 8
    hitW = 8
    hitH = 4

    override def update(): Unit = {

        state match {
            case Player.State.Normal => {
                if (input.inputX != 0) {
                    facingRight = input.inputX == 1
                }


                speedX = input.inputX.toFloat * Player.walkSpeed
                speedY = input.inputY.toFloat * Player.walkSpeed


                // lastDir is the handler for sprite direction
                input.inputY match
                    case -1 =>
                        lastDir = 1
                    case 1 =>
                        lastDir = 0
                    case 0 =>
                        input.inputX match
                            case -1 =>
                                lastDir = 2
                            case 1 =>
                                lastDir = 2
                            case 0 => ()

                spr = lastDir * 4
                if (input.inputX != 0 || input.inputY != 0) {
                    spr = lastDir * 4 + (Game.instance.gamemanager.currentState.frame / 11) % 4
                }




            }
        }

        moveX(speedX, Some(onCollideX))
        moveY(speedY, Some(onCollideY))

        if (speedY != 0) {
            zIndex = y
        }

        Game.instance.gamemanager.currentState.foreach {
            case obj: GayObject =>
                if (obj.canTouch(this) && obj.worldHitbox.overlaps(this.sniffZone)) {
                    obj.touch(this)
                }
            case _ => ()
        }

        graphic.asInstanceOf[GayAtlas].current = spr.toString
    }

    def sniffZone: gaymath.Rect =
        gaymath.Rect(x - 2, y - 1, 18, 18)




}
