

class Destructible extends GaySprite(GayTexture(Textures.destructible)) {
    hitX = 0
    hitY = 0
    hitW = 16
    hitH = 16
    solid = true

    override def canTouch(player: Player): Boolean = player.state == Player.State.Dash
    override def touch(player: Player): Unit = {
        if (player.state == Player.State.Dash)
            destroy()
    }
}
