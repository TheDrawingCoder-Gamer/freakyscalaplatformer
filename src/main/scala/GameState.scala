import scala.collection.{mutable => mut}
import gay.menkissing.common.math as gaymath
import scala.util.Using

import org.joml.Matrix4f
import org.lwjgl.opengl.*
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.system.*
import MemoryUtil.*
import MemoryStack.*

import GL11.*
import GL13.*
import GL15.*
import GL30.*
import GL20.*


class GayObject extends draw.Renderable {
    var visible: Boolean = true
    var active: Boolean = true
    var exists: Boolean = true

    var x: Int = 0
    var xRemainder: Float = 0
    var y: Int = 0
    var yRemainder: Float = 0

    var hitX: Int = 0
    var hitY: Int = 0
    var hitW: Int = 8
    var hitH: Int = 8

    var solid: Boolean = false
    var destroyed: Boolean = false

    var facingRight: Boolean = true

    var speedX: Float = 0
    var speedY: Float = 0

    def update(): Unit = ()
    def render(): Unit = ()
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
        val imin = math.floorDiv(hitbox.x, 8)
        val imax = math.floorDiv(hitbox.right - 1, 8)
        val jmin = math.floorDiv(hitbox.y, 8)
        val jmax = math.floorDiv(hitbox.bottom - 1, 8)
        val res = (imin to imax).exists { i =>
            (jmin to jmax).exists { j => 
                solidMap(i, j)
            }
        }
        if (res)
            return true

        return game.gamestate.objects.exists { it =>
            it.solid && it != this && !it.destroyed && hitbox.overlaps(it.worldHitbox)
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
    def destroy(): Unit = {
        // freaky!
        destroyed = true
    }
    def die(): Unit = ()

    def drawHitbox(): Unit = {
        val mtx = game.gamestate.camera.matrix(x + hitX, y + hitY, hitW, hitH, gaymath.PointF(1, 1), false)
       
        draw.setSolidColor(draw.Color(1, 0, 0, 1), mtx)
        draw.rect()
    }

    def touch(player: Player): Unit = ()
    def canTouch(player: Player): Boolean = false

    def squish(): Unit = die()
}



def solidMap(x: Int, y: Int): Boolean = {
    game.gamestate.world.tiles.fg.get(x, y).isDefined
}

sealed trait GayGraphic {
    def width: Int
    def height: Int

    def draw(matrices: Matrix4f): Unit
}

case class GayTexture(tex: draw.Texture) extends GayGraphic {
    def width = tex.width
    def height = tex.height
    def draw(matrices: Matrix4f): Unit = {
        tex.draw(matrices, 0, 0, tex.width, tex.height)
    }
}

class GayAtlas(val atlas: draw.TextureAtlas, var current: String) extends GayGraphic {
    def width = atlas.get(current).get.w
    def height = atlas.get(current).get.h
    def draw(matrices: Matrix4f): Unit = {
        atlas.draw(matrices, current)
    }
}


class GaySprite(var graphic: GayGraphic) extends GayObject {
    var scrollFactor: gaymath.PointF = gaymath.PointF(1, 1)
    override def render(): Unit =  {
        val mtx = game.gamestate.camera.matrix(x, y, graphic.width, graphic.height, scrollFactor, !facingRight)
    
        graphic.draw(mtx)
    }

}

class TestObject(texture: draw.Texture) extends GaySprite(GayTexture(texture)) {
    var counter: Double = 0
    override def update() = {
        counter = counter + 0.2
        x = (counter % renderWidth).toInt
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

class GayCamera(x: Int, y: Int) extends gaymath.Point(x, y) {
    def matrix(px: Int, py: Int, pw: Int, ph: Int, scrollFactor: gaymath.PointF, flipX: Boolean): Matrix4f = {
        val mtx = Matrix4f()
        mtx.ortho(0.25f, renderWidth + 0.25f, renderHeight - 0.25f, -0.25f, -1, 1)
        mtx.translate(px.toFloat - (x.toFloat * scrollFactor.x.toFloat), py.toFloat - (y.toFloat * scrollFactor.y.toFloat), 0)
        if (flipX) {
            mtx.translate(pw.toFloat, 0, 0)
            mtx.scale(-1, 1, 1)
        }
        mtx.scale(pw, ph, 1)
    }
    def matrixStack(px: Int, py: Int, pw: Int, ph: Int, scrollFactor: gaymath.PointF, flipX: Boolean): draw.MatrixStack = {
        val mtx = matrix(px, py, pw, ph, scrollFactor, flipX)
        val stack = draw.MatrixStack()
        stack.set(mtx)
        stack
    }
    def inScreenSpace(px: Int, py: Int, scrollFactor: gaymath.PointF = gaymath.PointF(1, 1)): gaymath.PointF = {
        gaymath.PointF(px.toFloat + (x.toFloat * scrollFactor.x), py.toFloat - (y.toFloat * scrollFactor.y))
    }
}


class GameState() {
    Textures
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    val objects = mut.ListBuffer[GayObject]()
    var camera = GayCamera(0, 0)
    val input1 = Input(0)
    val world = World.load()
    val framebufferTex = glGenTextures()
    var frame = 0
    glBindTexture(GL_TEXTURE_2D, framebufferTex)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP)
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, draw.renderWidth, draw.renderHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, 0)
    glBindTexture(GL_TEXTURE_2D, 0)
    val rbo = glGenRenderbuffers()
    glBindRenderbuffer(GL_RENDERBUFFER, rbo)
    glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT, draw.renderWidth, draw.renderHeight)
    glBindRenderbuffer(GL_RENDERBUFFER, 0)
    val framebuffer = glGenFramebuffers()
    glBindFramebuffer(GL_FRAMEBUFFER, framebuffer)
    glFramebufferTexture2D(GL_FRAMEBUFFER,
                        GL_COLOR_ATTACHMENT0,
                        GL_TEXTURE_2D,
                        framebufferTex,
                        0)
    glFramebufferRenderbuffer(GL_FRAMEBUFFER,
                        GL_DEPTH_ATTACHMENT,
                        GL_RENDERBUFFER,
                        rbo)
    glBindFramebuffer(GL_FRAMEBUFFER, 0)
    val screenTransform = Matrix4f()
    objects.append { 
        val player = new Player(input1) 
        player.x = world.start.pos.x * 8
        player.y = world.start.pos.y * 8
        player
    }
    world.levels(world.start.level).addEntities(objects)
    def clean(killPlayer: Boolean = false): Unit = {
        for (obj <- objects) {
            if (killPlayer || !obj.isInstanceOf[Player])
                obj.destroy()
        }
        if (killPlayer)
            objects.clear()
        else {
            val player = objects.find(_.isInstanceOf[Player])
            objects.clear()
            objects.appendAll(player)
        }


    }
    def run(): Unit = {
        glBindFramebuffer(GL_FRAMEBUFFER, framebuffer)
        glViewport(0, 0, renderWidth, renderHeight)
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
        val stack = draw.MatrixStack()
        
        stack.ortho(0, renderWidth, renderHeight, 0, -1, 1)
        input1.update()
        drawMap(stack)
        objects.foreach { obj => 
            obj.update()
            obj.render()
        }
        objects.filterInPlace(!_.destroyed)
        glBindFramebuffer(GL_FRAMEBUFFER, 0)
        glViewport(0, 0, windowWidth, windowHeight)

        glUseProgram(draw.Shaders.defaultProgram)
        glBindVertexArray(draw.screenVertices.VAO)
        glActiveTexture(GL_TEXTURE0)
        glBindTexture(GL_TEXTURE_2D, framebufferTex)


        draw.bindTransform(draw.Shaders.defaultProgram, screenTransform, Matrix4f())

        glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0)
        frame += 1
        frame %= 32767

    }
    def drawMap(stack: draw.MatrixStack): Unit = stack.scoped {
        stack.translate(-camera.x, -camera.y, 0)
        val ccx = math.floorDiv(camera.x, 8)
        val ccy = math.floorDiv(camera.y, 8)

        val xOff = -(camera.x % 8)
        val yOff = -(camera.y % 8)

        val minx = ccx
        val miny = ccy

        val maxx = ccx + 42
        val maxy = ccy + 25

        for (i <- minx until maxx) {
            for (j <- miny until maxy) {
                val bgTile = world.tiles.bg.get(i, j)
                val fgTile = world.tiles.fg.get(i, j)
                stack.scoped {
                    stack.translate((i - minx) * 8, (j - miny) * 8, 0)
                    stack.scaleXY(8, 8)
                    bgTile.foreach { tile =>
                        Textures.tiles.draw(stack, (tile % 16) * 8, math.floorDiv(tile, 16) * 8, 8, 8)
                    }
                    fgTile.foreach { tile =>
                        Textures.tiles.draw(stack, (tile % 16) * 8, math.floorDiv(tile, 16) * 8, 8, 8)
                    }
                }
            }
        }
    }
}
