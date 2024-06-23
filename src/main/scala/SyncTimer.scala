import org.lwjgl.glfw.GLFW.glfwGetTime

object SyncTimer {
    val resolution = 1
}
class SyncTimer {
    var timeThen: Double = glfwGetTime()
    var enabled = true
    def resolution: Double = 1.0
    def sync(fps: Double): Int = {
        var updates: Int = 0
        var timeNow = glfwGetTime()

        if (enabled) {
            var gapTo: Double = resolution / fps + timeThen

            while (gapTo < timeNow) {
                gapTo = resolution / fps + gapTo
                updates += 1
            }
            while (gapTo > timeNow) {
                Thread.sleep(1)
                timeNow = glfwGetTime()
            }
            updates += 1

            timeThen = gapTo
        } else {
            while (timeThen < timeNow) {
                timeThen = resolution / fps + timeThen
                updates += 1
            }
        }

        updates
    }
}
