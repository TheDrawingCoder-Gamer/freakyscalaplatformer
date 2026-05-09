package gay.menkissing

import engine.graphics.TexturePack

object Textures {
    val preload = new PreloadTextures()

}

final class PreloadTextures extends TexturePack {
    val playerSheet = engine.graphics.Texture(getClass.getResourceAsStream("/player.png")).register()
    val tiles = engine.graphics.Texture(getClass.getResourceAsStream("/tiles.png")).register()
    val destructible = engine.graphics.Texture(getClass.getResourceAsStream("/destructible.png")).register()
    val haxe = engine.graphics.Texture(getClass.getResourceAsStream("/haxe1.png")).register()
    val elements = engine.graphics.Texture(getClass.getResourceAsStream("/elements.png")).register()
    val lucaPortrait = engine.graphics.Texture(getClass.getResourceAsStream("/lucaportrait.png")).register()
    val testEnemy = engine.graphics.Texture(getClass.getResourceAsStream("/testenemy.png")).register()
}