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

    val dashSpeed = 5f
    val endDashSpeed = 3.5f
    val wallBounceSpeed = -4f
    val wallBounceHorzSpeed = 2f
    val dashJumpSpeed = -2f
    val dashJumpHorzMult = 0.5f
    val wallJumpHorzSpeed = 3
    val wallJumpVertSpeed = -3
    var superDashSpeed = 5f

    val playerAtlas = draw.TextureAtlas.splitBySize(Textures.playerSheet, 16, 16)

    val rechargeColor = draw.Color.fromHex(0xFFFFFFFF)
    val unkDashColor = draw.Color.fromHex(0xFF000000)
    val dashColors = Vector(
        draw.Color.fromHex(0xFF107EE5),
        draw.Color.fromHex(0xFFE51061)
        )
}

class Player(val input: Input) extends GayObject, draw.Renderable {
    var state: Player.State = Player.State.Normal 
    var tJumpGrace: Int = 0
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
    var recharging: Boolean = false
    var tRecharge: Int = 0
    var downDash: Boolean = false
    var tDashTime: Int = 0

    hitX = 2
    hitY = 2
    hitW = 5
    hitH = 14

    override def update() = {
        val onGround = checkSolid(0, 1)

        if (onGround) {
            tJumpGrace = 8
            jumpGraceY = y
        } else {
            if (tJumpGrace > 0) {
                tJumpGrace -= 1
            }
        }
        if (tRecharge > 0) {
            tRecharge -= 1
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

                if (!onGround) {
                    recharging = false
                    tRecharge = 0
                } else {
                    if (dashes < maxDashes && tRecharge == 0) {
                        if (recharging) {
                            recharging = false
                            refillDashes()
                        } else {
                            recharging = true
                            tRecharge = 6
                        }
                    }
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
                    if(tJumpGrace > 0) {
                        jump()
                    }
                }

                if (input.inputActionPressed > 0) {
                    if (dashes > 0) {
                        dash()
                    }
                } 

            }
            case Player.State.Death => {}
            case Player.State.Dash => {
                tDashTime -= 1

                if (downDash && onGround) {
                    crouching = true
                }

                if (input.inputJumpPressed > 0) {
                    lazy val touchedWall = touchingWall()
                    if (!onGround && touchedWall != 0) {
                       wallBounce(touchedWall) 
                    } else if (tJumpGrace > 0) {
                        if (crouching)
                            dashJump(onGround)
                        else
                            superDash(onGround)
                    }
                }

                if (tDashTime == 0)
                    endDash()
            }
        }

        moveX(speedX, Some(onCollideX))
        moveY(speedY, Some(onCollideY))
    }

    def rawJump(_playSound: Boolean): Boolean = {
        input.consumeJumpPress()
        state = Player.State.Normal
        speedY = Player.jumpSpeed
        varJumpSpeed = Player.jumpSpeed
        speedX = input.inputX.toFloat * Player.jumpHorzSpeedMultiplier
        tVarJump = 4
        tJumpGrace = 0
        autoVarJump = false

        false
    }

    def jump(): Unit = rawJump(true)

    def resetDashInfo(): Unit = {
        recharging = false
        tRecharge = 0
    }

    def wallBounce(dir: Int): Unit = {
        input.consumeJumpPress()
        state = Player.State.Normal
        speedY = Player.wallBounceSpeed
        varJumpSpeed = Player.wallBounceSpeed
        speedX = dir.toFloat * -Player.wallBounceHorzSpeed
        tVarJump = 4
        autoVarJump = false
        facingRight = dir != 1
        moveX(-dir * Player.wallBounceHorzSpeed, None)
    }
    def refillDashes(): Unit = {
        dashes = maxDashes
    }
    def superDash(recover: Boolean): Unit = {
        speedX = math.signum(speedX) * Player.superDashSpeed
        rawJump(false)
        if (recover)
            refillDashes()
        resetDashInfo()
    }
    def dashJump(recover: Boolean): Unit = {
        input.consumeJumpPress()
        state = Player.State.Normal
        speedY = Player.jumpSpeed
        varJumpSpeed = Player.jumpSpeed
        speedX = speedX + input.inputX.toFloat * Player.dashJumpHorzMult
        tVarJump = 4
        tJumpGrace = 0
        autoVarJump = false
        resetDashInfo()
        if (recover)
            refillDashes()
        moveY(jumpGraceY - y, None)
        // todo: platforms
    }
    def wallJump(dir: Int): Unit = {
        input.consumeJumpPress()
        state = Player.State.Normal
        speedY = Player.wallJumpVertSpeed
        varJumpSpeed = Player.wallJumpVertSpeed
        speedX = Player.wallJumpHorzSpeed * dir.toFloat
        tVarJump = 4
        autoVarJump = false
        facingRight = dir > 0
        moveX(-dir.toFloat * Player.wallJumpHorzSpeed, None)
    }
    def dash(): Unit = {
        input.consumeActionPress()
        state = Player.State.Dash
        dashes -= 1
        resetDashInfo()
        downDash = input.inputY > 0
        val xDir =
            if (input.inputX == 0 && input.inputY == 0)
                if (facingRight) 1 else -1
            else
                input.inputX.toInt
        val xSpeed: Float = 
            if (xDir != 0 && math.signum(speedX).toInt == xDir)
                if (xDir == 1)
                    math.max(Player.dashSpeed, speedX)
                else
                    math.min(-Player.dashSpeed, speedX)
            else
                Player.dashSpeed * xDir.toFloat
        speedX = xSpeed
        speedY = input.inputY.toFloat * Player.dashSpeed

        if (input.inputY < 0) {
            tJumpGrace = 0
        }
        tDashTime = 8
    }
    def endDash(): Unit = {
        state = Player.State.Normal
        if (!downDash) {
            speedX = Player.endDashSpeed * math.signum(speedX)
            speedY = Player.endDashSpeed * math.signum(speedY)
        }
    }

    def touchingWall(): Int = {
        if (checkSolid(-1, 0))
            -1
        else if (checkSolid(1, 0))
            1
        else
            0
    }

    override def render(): Unit = {
        // freaky!
        val matrices = draw.MatrixStack() 

        matrices.ortho(0, renderWidth, renderHeight, 0, -1, 1)
        val screenSpace = game.gamestate.camera.inScreenSpace(x, y)
        matrices.translate(screenSpace.x.toFloat, screenSpace.y.toFloat, 0)

        if (!facingRight) {
            matrices.translate(16, 0, 0)
            matrices.scale(-1, 1, 1)
        }
        matrices.scoped {
            matrices.translate(7 + skullOffsetX, 5 + skullOffsetY, 0)
            matrices.scaleXY(5, 5)
            val color = if (recharging) Player.rechargeColor else Player.dashColors.lift(dashes).getOrElse(Player.unkDashColor)
            draw.setSolidColor(color, matrices)
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
