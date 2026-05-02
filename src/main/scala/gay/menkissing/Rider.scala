package gay.menkissing

import gay.menkissing.common.math as gaymath
import gay.menkissing.engine.GayObject
// you know who else rides?
// me :3
trait GayRider {
    def rideCheck(platform: GayObject): Boolean
    def rideSetVelocity(value: gaymath.PointF): Unit
}
