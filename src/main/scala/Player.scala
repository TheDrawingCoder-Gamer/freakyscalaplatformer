import gay.menkissing.common.math as gaymath

object Player {
    enum State {
        case Normal, Death, Dash
    }
    val maxFallSpeed = 1.4f
    val maxFastFallSpeed = 1.8f
    val gravity = 0.8f
    val floatGravity = 0.2f
    val floatThreshold = 0.2f
    val jumpSpeed = -4.5f
    val jumpHorzSpeedMultiplier = 0.2f
    
    val walkSpeed = 1f

}

class Player(val input: Input) extends GaySprite(GayAtlas(draw.TextureAtlas.split(Textures.playerSheet, 4, 1), "0")) {
    var state: Player.State = Player.State.Normal 
    var jumpGrace: Int = 0
    var jumpGraceY: Int = 0
    var crouching: Boolean = false
    var tVarJump: Int = 0
    var autoVarJump: Boolean = false
    var varJumpSpeed: Float = 0

    hitX = 2
    hitY = 2
    hitW = 5
    hitH = 6

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


        state match {
            case Player.State.Normal => {
                if (input.inputX != 0) {
                    facingRight = input.inputX == 1
                }

                var accel: Float = 0.2

                if (math.abs(speedX) > 2 && math.signum(input.inputX) == math.signum(speedX)) {
                    accel = 0.05
                } else if (onGround) {
                    accel = if (crouching) 0.25 else 0.4
                }

                speedX = gaymath.approach(speedX, input.inputX.toFloat * Player.walkSpeed, accel).toFloat

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
}
