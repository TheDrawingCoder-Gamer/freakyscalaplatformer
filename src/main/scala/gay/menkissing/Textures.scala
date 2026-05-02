package gay.menkissing

import engine.graphics.TexturePack

object Textures {
    val preload = new PreloadTextures()

}

final class PreloadTextures extends TexturePack {
    val playerSheet = draw.Texture(getClass.getResourceAsStream("/player.png")).register()
    val tiles = draw.Texture(getClass.getResourceAsStream("/tiles.png")).register()
    val destructible = draw.Texture(getClass.getResourceAsStream("/destructible.png")).register()
    val haxe = draw.Texture(getClass.getResourceAsStream("/haxe1.png")).register()

}