import org.lwjgl.*
import org.lwjgl.system.*
import org.lwjgl.glfw.*
import org.lwjgl.glfw.GLFW.*

import org.lwjgl.system.MemoryStack.*
import org.lwjgl.system.MemoryUtil.*

import scala.util.Using

class Input(val player: Byte) {
  var inputX: Byte = 0
  var inputY: Byte = 0
  var inputJump: Boolean = false
  var inputJumpPressed: Byte = 0
  var inputAction: Boolean = false
  var inputActionPressed: Byte = 0
  var inputGun: Boolean = false
  var inputGunPressed: Byte = 0
  var axisXValue: Byte = 0
  var axisXTurned: Boolean = false
  var axisYValue: Byte = 0
  var axisYTurned: Boolean = false
  var inputLockHeld: Boolean = false
  var gamepadId: Int = -1
    
  def consumeJumpPress(): Boolean = {
    val res = inputJumpPressed > 0
    inputJumpPressed = 0
    res
  }
  def update(): Unit = Using.resource(stackPush()) { stack =>
    val prevX: Byte = axisXValue
    val prevY: Byte = axisYValue
    val state = GLFWGamepadState.calloc(stack)
    if (gamepadId != -1) {
      glfwGetGamepadState(gamepadId, state) 
    }
    val left = 
        (player == 0 && glfwGetKey(game.window, GLFW_KEY_LEFT) == GLFW_PRESS) || state.buttons(GLFW_GAMEPAD_BUTTON_DPAD_LEFT) != 0
    val right = 
        (player == 0 && glfwGetKey(game.window, GLFW_KEY_RIGHT) == GLFW_PRESS) || state.buttons(GLFW_GAMEPAD_BUTTON_DPAD_RIGHT) != 0
    val up = 
        (player == 0 && glfwGetKey(game.window, GLFW_KEY_UP) == GLFW_PRESS) || state.buttons(GLFW_GAMEPAD_BUTTON_DPAD_UP) != 0
    val down = 
        (player == 0 && glfwGetKey(game.window, GLFW_KEY_DOWN) == GLFW_PRESS) || state.buttons(GLFW_GAMEPAD_BUTTON_DPAD_DOWN) != 0
    val jump = 
        (player == 0 && glfwGetKey(game.window, GLFW_KEY_Z) == GLFW_PRESS) || state.buttons(GLFW_GAMEPAD_BUTTON_A) != 0
    val action = 
        (player == 0 && glfwGetKey(game.window, GLFW_KEY_X) == GLFW_PRESS) || state.buttons(GLFW_GAMEPAD_BUTTON_X) != 0
    val gun = 
        (player == 0 && glfwGetKey(game.window, GLFW_KEY_C) == GLFW_PRESS) || state.buttons(GLFW_GAMEPAD_BUTTON_B) != 0
    val lock = 
      (player == 0 && glfwGetKey(game.window, GLFW_KEY_LEFT_SHIFT) == GLFW_PRESS) || state.axes(GLFW_GAMEPAD_AXIS_RIGHT_TRIGGER) > 0.8
    if (left) {
        if (right) {
            if (axisXTurned) {
                axisXValue = prevX;
                inputX = prevX;
            } else {
                axisXTurned = true;
                axisXValue = (-prevX).toByte;
                inputX = (-prevX).toByte;
            }
        } else {
            axisXTurned = false;
            axisXValue = -1;
            inputX = -1;
        }
    } else if (right) {
        axisXTurned = false;
        axisXValue = 1;
        inputX = 1;
    } else {
        axisXTurned = false;
        axisXValue = 0;
        inputX = 0;
    }
    if (up) {
        if (down) {
            if (axisYTurned) {
                axisYValue = prevY;
                inputY = prevY;
            } else {
                axisYTurned = true;
                axisYValue = (-prevY).toByte;
                inputY = (-prevY).toByte;
            }
        } else {
            axisYTurned = false;
            axisYValue = -1;
            inputY = -1;
        }
    } else if (down) {
        axisYTurned = false;
        axisYValue = 1;
        inputY = 1;
    } else {
        axisYTurned = false;
        axisYValue = 0;
        inputY = 0;
    }

    if (jump && !inputJump) {
        inputJumpPressed = 8;
    } else {
        if (jump) {
            if (inputJumpPressed > 0)
                inputJumpPressed = (inputJumpPressed - 1).toByte;
        } else {
            inputJumpPressed = 0;
        }
    }
    inputJump = jump;

    if (action && !inputAction) {
        inputActionPressed = 8;
    } else {
        if (action) {
            if (inputActionPressed > 0)
                inputActionPressed = (inputActionPressed - 1).toByte;
        } else {
            inputActionPressed = 0;
        }
    }
    inputAction = action;

    if (gun && !inputGun) {
        inputGunPressed = 8;
    } else {
        if (gun) {
            if (inputGunPressed > 0)
                inputGunPressed = (inputGunPressed - 1).toByte;
        } else {
            inputGunPressed = 0;
        }
    }
    inputGun = gun;

    inputLockHeld = lock;
  }

}
