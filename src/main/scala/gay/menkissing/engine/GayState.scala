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

abstract class GayState(val manager: GameManager) extends GayTypedContainer[GayBasic] {
    def start(): Unit


    def close(): Unit

    def frame: Int = manager.frame
}