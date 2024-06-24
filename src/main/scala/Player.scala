import gay.menkissing.common.math as gaymath
import org.lwjgl.opengl.*
import org.joml.Matrix4f

import GL11.*
import GL15.*
import GL20.*
import GL13.*
import GL30.*

object Player {
    enum State {
        case Normal, Death, Dash
    }
    val maxFallSpeed = 2.8f
    val maxFastFallSpeed = 3.4f
    val gravity = 0.8f
    val floatGravity = 0.2f
    val floatThreshold = 0.2f
    val jumpSpeed = -4.5f
    val jumpHorzSpeedMultiplier = 0.2f
    
    val walkSpeed = 1.5f

    val dashSpeed = 3f
    val endDashSpeed = 2f
    val wallBounceSpeed = -4f
    val wallBounceHorzSpeed = 2f
    val dashJumpSpeed = -2f
    val dashJumpHorzMult = 0.5
    val wallJumpHorzSpeed = 3
    val wallJumpVertSpeed = -3

    val playerAtlas = draw.TextureAtlas.splitBySize(Textures.playerSheet, 16, 16)

    val unkDashColor = draw.Color.fromHex(0xFF000000)
    val dashColors = Vector(
        draw.Color.fromHex(0xFF107EE5),
        draw.Color.fromHex(0xFFE51061)
        )
}

class Player(val input: Input) extends GayObject, draw.Renderable {
    var state: Player.State = Player.State.Normal 
    var jumpGrace: Int = 0
    var jumpGraceY: Int = 0
    var crouching: Boolean = false
    var tVarJump: Int = 0
    var autoVarJump: Boolean = false
    var varJumpSpeed: Float = 0
    var spr: Int = 0

    var skullOffsetX: Int = 0
    var skullOffsetY: Int = 0

    var dashes: Int = 1
    var maxDashes: Int = 1

    hitX = 2
    hitY = 2
    hitW = 5
    hitH = 14

    def update() = {
        val onGround = checkSolid(0, 1)

        if (onGround) {
            jumpGrace = 8
            jumpGraceY = y
        } else {
            if (jumpGrace > 0) {
                jumpGrace -= 1
            }
        }

        skullOffsetX = 0
        skullOffsetY = 0

        state match {
            case Player.State.Normal => {
                if (input.inputX != 0) {
                    facingRight = input.inputX == 1
                }

                if (onGround) {
                    crouching = input.inputY > 0
                }
                var accel: Float = 0.2

                if (math.abs(speedX) > 2 && math.signum(input.inputX) == math.signum(speedX)) {
                    accel = 0.05
                } else if (onGround) {
                    accel = if (crouching) 0.25 else 0.4
                }

                speedX = gaymath.approach(speedX, input.inputX.toFloat * Player.walkSpeed, accel).toFloat

                if (crouching) {
                    spr = 4
                    skullOffsetY = 1
                } else if (!onGround) {
                    spr = 2
                } else {
                    spr = 0
                    if (input.inputX != 0) {
                        spr = (game.gamestate.frame / 15) % 2
                    } 
                }
                val maxFall = if (input.inputY > 0) Player.maxFastFallSpeed else Player.maxFallSpeed

                if (math.abs(speedY) < Player.floatThreshold && input.inputJump) {
                    speedY = math.min(speedY + Player.floatGravity, maxFall).toFloat
                } else {
                    speedY = math.min(speedY + Player.gravity, maxFall).toFloat
                }


                if (tVarJump > 0) {
                    if (input.inputJump || autoVarJump) {
                        speedY = varJumpSpeed
                        tVarJump -= 1
                    } else {
                        tVarJump = 0
                    }
                }

                if (input.inputJumpPressed > 0) {
                    if(jumpGrace > 0) {
                        jump()
                    }
                }

                

            }
            case Player.State.Death => {}
            case Player.State.Dash => {}
        }

        moveX(speedX, Some(onCollideX))
        moveY(speedY, Some(onCollideY))
    }

    def jump(): Unit = {
        input.consumeJumpPress()
        state = Player.State.Normal
        speedY = Player.jumpSpeed
        varJumpSpeed = Player.jumpSpeed
        speedX = input.inputX.toFloat * Player.jumpHorzSpeedMultiplier
        tVarJump = 4
        jumpGrace = 0
        autoVarJump = false
    }

    def render(ctx: draw.GraphicsContext): Unit = ctx.stack.translated(
        x.toFloat - ctx.camera.x.toFloat,
        y.toFloat - ctx.camera.y.toFloat
        ) {
        // freaky!
        val matrices = ctx.stack

        if (!facingRight) {
            matrices.translate(16, 0, 0)
            matrices.scale(-1, 1, 1)
        }
        matrices.scoped {
            matrices.translate(7 + skullOffsetX, 5 + skullOffsetY, 0)
            matrices.scaleXY(5, 5)
            draw.setSolidColor(Player.dashColors.lift(dashes).getOrElse(Player.unkDashColor), matrices)
            draw.filledCircle()
        }
        matrices.scoped {
            matrices.scaleXY(16, 16)
            Player.playerAtlas.draw(matrices, spr.toString)
        }

        matrices.translated(skullOffsetX, skullOffsetY) {
            matrices.scaleXY(16, 16)
            Player.playerAtlas.draw(matrices, "3")
        }
    }
}
