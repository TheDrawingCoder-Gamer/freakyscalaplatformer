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
import gay.menkissing.engine.group.{GayContainer, GayTypedContainer}

open class GayBasic {
    var visible: Boolean = true
    var active: Boolean = true
    var exists: Boolean = true

    protected var _layer: Option[GayView] = None

    def layer: GayView = _layer.getOrElse(Game.instance.gamemanager.cameras.defaultCam)
    // cam is nullable
    def setLayer(cam: GayView | Null): Unit = _layer = Option(cam)

    var destroyed: Boolean = false

    protected var _container: Option[GayContainer] = None
    def container: Option[GayContainer] = _container
    def container_=(v: Option[GayContainer]): Unit = _container = v

    def update(): Unit = ()
    def render(): Unit = ()

    def collectForRender(): Unit =
      if (this.exists && this.visible)
        this.layer.submitForRender(this)

    def destroy(): Unit = {
        // freaky!
        destroyed = true
    }
}