package gay.menkissing.battle

import gay.menkissing.Textures
import gay.menkissing.battle.BattleMenu.{BattleOption, SkillBattleOption}
import gay.menkissing.engine.*
import gay.menkissing.draw.{TextureAtlas}
import gay.menkissing.engine.graphics.Color
import gay.menkissing.engine.group.{GayObjectContainer, GayTypedObjectContainer}

abstract class BattleMenu extends GayTypedObjectContainer[GayObject] {
  val options: Vector[BattleOption]


  protected def updateOptions(): Unit =
    options.zipWithIndex.foreach { (x, i) =>
      x.y = BattleMenu.optionHeight * i
    }
    editSelection(0)


  // Where will this menu loop scrolling?
  def length: Int = options.length
  var selectedOption: Int = 0

  def currentSelected: BattleOption = options(selectedOption)

  def editSelection(by: Int): Unit =
    options(selectedOption).hovered = false
    selectedOption += by
    selectedOption = math.floorMod(selectedOption, length)
    options(selectedOption).hovered = true


}

class RootBattleMenu extends BattleMenu {
  val attackOption = BattleOption(L10n.instance.named("menu.battle.attack"))
  attackOption.hovered = true
  add(attackOption)
  val skillOption = BattleOption(L10n.instance.named("menu.battle.skills"))
  add(skillOption)
  val itemOption = BattleOption(L10n.instance.named("menu.battle.item"))
  add(itemOption)

  val options: Vector[BattleOption] = Vector(attackOption, skillOption, itemOption)

  updateOptions()

}

object RootBattleMenu {
  object Options {
    val Attack = 0
    val Skill = 1
    val Item = 2
  }
}

class SkillBattleMenu(val root: RootBattleMenu) extends BattleMenu {
  val options: Vector[SkillBattleOption] = Vector.fill[SkillBattleOption](8)(SkillBattleOption())
  options.foreach(this.add)

  override def currentSelected: SkillBattleOption =  options(selectedOption)

  var nSkills: Int = 1

  override def length: Int = nSkills
  updateOptions()

  def loadSkills(skills: List[Skill]): Unit = {
    options.foreach(_.skill = None)
    skills.zipWithIndex.foreach { (skill, i) =>
      assert(i < 8)
      // TODO: display elem graphic
      options(i).skill = Some(skill)
      this.nSkills = i
    }
    this.nSkills += 1
    editSelection(0)
  }

}

object BattleMenu {
  val primaryColor: Color = Color.fromHex(0xFFEEB902)
  val secondaryColor: Color = Color.fromHex(0xFF25283D)
  val optionHeight: Int = 42

  val elementAtlas = TextureAtlas.splitBySize(Textures.preload.elements, 32, 32)

  class BattleOption(text: String) extends GayTypedObjectContainer[GayObject] {
    private var _hovered: Boolean = false

    val txt = GayText(text, 32, secondaryColor)
    txt.y = 0
    txt.x = 10
    val rect = GayRectSprite(400, optionHeight, primaryColor)

    add(rect)
    add(txt)

    def hovered: Boolean = _hovered
    def hovered_=(v: Boolean): Unit =
      _hovered = v
      if (v) {
        txt.color = primaryColor
        rect.setColor(secondaryColor)
      } else {
        txt.color = secondaryColor
        rect.setColor(primaryColor)
      }

  }

  class SkillBattleOption extends BattleOption("") {
    txt.x += 32
    private var _skill: Option[Skill] = None
    val skillCostTxt = GayText("", 32, secondaryColor)

    def skill: Option[Skill] = _skill
    def skill_=(v: Option[Skill]): Unit =
      _skill = v
      v match
        case Some(value) =>
          skillIcon.visible = true
          skillGraphic.current = value.element.ordinal.toString
          txt.text = value.name
        case None =>
          skillIcon.visible = false
          txt.text = ""

    val skillGraphic = GayAtlas(elementAtlas, "0")
    val skillIcon = GaySprite(skillGraphic)
    skillIcon.visible = false
    skillIcon.x = 10
    skillIcon.y = 4

    add(skillIcon)

  }

  
  final class RoundedRectangle(var radius: Int,
                               override var graphicalWidth: Int,
                               override var graphicalHeight: Int) extends GayObject {
    override def render(): Unit = {
      Game.instance.gamemanager.cameras.camList.withFilter(this.viewableOnCamera)
    }
  }

  final class SkillCostPreview extends GayObjectContainer {

  }
}