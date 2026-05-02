package gay.menkissing.battle

import gay.menkissing.battle.BattleMenu.BattleOption
import gay.menkissing.engine.*
import gay.menkissing.draw.Color

class BattleMenu extends GayTypedObjectContainer[GayObject] {
  val attackOption = BattleOption("Attack")
  attackOption.hovered = true
  add(attackOption)
  val skillOption = BattleOption("Persona")
  add(skillOption)
  val itemOption = BattleOption("Item")
  add(itemOption)

  val options = Vector(attackOption, skillOption, itemOption)
  options.zipWithIndex.foreach { (x, i) =>
    x.y = BattleMenu.optionHeight * i
  }
  var selectedOption: Int = 0

  def editSelection(by: Int): Unit =
    options(selectedOption).hovered = false
    selectedOption += by
    selectedOption = math.floorMod(selectedOption, options.length)
    options(selectedOption).hovered = true

  override def update(): Unit =
    super.update()

    if (GayG.input.dPadDown.justPressed) {
      editSelection(1)
    } else if (GayG.input.dPadUp.justPressed) {
      editSelection(-1)
    }



}

object BattleMenu {
  val primaryColor: Color = Color.fromHex(0xFFEEB902)
  val secondaryColor: Color = Color.fromHex(0xFF25283D)
  val optionHeight: Int = 42

  final class BattleOption(text: String) extends GayTypedObjectContainer[GayObject] {
    private var _hovered: Boolean = false

    val txt = GayText(text, 32, secondaryColor)
    txt.y = 4
    txt.x = 10
    val rectGraphic = GayRect(400, optionHeight, primaryColor)
    val rect = GaySprite(rectGraphic)

    add(rect)
    add(txt)

    def hovered: Boolean = _hovered
    def hovered_=(v: Boolean): Unit =
      _hovered = v
      if (v) {
        txt.color = primaryColor
        rectGraphic.color = secondaryColor
      } else {
        txt.color = secondaryColor
        rectGraphic.color = primaryColor
      }

  }
}