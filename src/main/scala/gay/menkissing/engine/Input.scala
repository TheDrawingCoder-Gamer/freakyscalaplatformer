package gay.menkissing.engine

import org.lwjgl.*
import org.lwjgl.glfw.*
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.system.*
import org.lwjgl.system.MemoryStack.*
import org.lwjgl.system.MemoryUtil.*

import scala.util.Using

class Input(val player: Byte) {
  var inputX: Byte = 0
  var inputY: Byte = 0
  val dPadUp = Input.BindSlot()
  val dPadDown = Input.BindSlot()
  val dPadLeft = Input.BindSlot()
  val dPadRight = Input.BindSlot()
  val confirm = Input.BindSlot()
  val cancel = Input.BindSlot()
  var axisXValue: Byte = 0
  var axisXTurned: Boolean = false
  var axisYValue: Byte = 0
  var axisYTurned: Boolean = false
  var gamepadId: Int = -1
  
  def update(): Unit = Using.resource(stackPush()) { stack =>
    val prevX: Byte = axisXValue
    val prevY: Byte = axisYValue
    val state = GLFWGamepadState.calloc(stack)
    if (gamepadId != -1) {
      glfwGetGamepadState(gamepadId, state) 
    }
    val left = 
        (player == 0 && glfwGetKey(Game.instance.window, GLFW_KEY_LEFT) == GLFW_PRESS) || state.buttons(GLFW_GAMEPAD_BUTTON_DPAD_LEFT) != 0
    val right = 
        (player == 0 && glfwGetKey(Game.instance.window, GLFW_KEY_RIGHT) == GLFW_PRESS) || state.buttons(GLFW_GAMEPAD_BUTTON_DPAD_RIGHT) != 0
    val up = 
        (player == 0 && glfwGetKey(Game.instance.window, GLFW_KEY_UP) == GLFW_PRESS) || state.buttons(GLFW_GAMEPAD_BUTTON_DPAD_UP) != 0
    val down = 
        (player == 0 && glfwGetKey(Game.instance.window, GLFW_KEY_DOWN) == GLFW_PRESS) || state.buttons(GLFW_GAMEPAD_BUTTON_DPAD_DOWN) != 0
    val confirm = 
        (player == 0 && glfwGetKey(Game.instance.window, GLFW_KEY_Z) == GLFW_PRESS) || state.buttons(GLFW_GAMEPAD_BUTTON_A) != 0
    val cancel = 
        (player == 0 && glfwGetKey(Game.instance.window, GLFW_KEY_X) == GLFW_PRESS) || state.buttons(GLFW_GAMEPAD_BUTTON_X) != 0
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
    this.dPadDown.update(down)
    this.dPadLeft.update(left)
    this.dPadRight.update(right)
    this.dPadUp.update(up)
    this.confirm.update(confirm)
    this.cancel.update(cancel)
    
  }

}

object Input {
  final class BindSlot {
    var active: Boolean = false
    var justPressed: Boolean = false
    
    def update(newInput: Boolean): Unit =
      if (newInput && !active) {
        justPressed = true
      } else {
        justPressed = false
      }
      active = newInput
      
    def consume(): Boolean =
      val r = justPressed
      justPressed = false
      r
  }
  
  
}