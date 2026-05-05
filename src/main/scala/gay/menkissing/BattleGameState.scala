package gay.menkissing

import gay.menkissing.battle.{BattleMenu, RootBattleMenu, Skill, SkillBattleMenu, Skills}
import gay.menkissing.engine.{GameManager, GayG, GayState, GayText}
import gay.menkissing.engine.audio.SoundSource

enum MenuFocus {
  case Root
  case Skill
}

class BattleGameState(manager: GameManager) extends GayState(manager):
  val menu =RootBattleMenu()
  menu.cameras.hideFromDefault().addToCamera(manager.fullCam)
  val skillMenu = SkillBattleMenu(menu)
  skillMenu.loadSkills(List(Skills.nuke.weak, Skills.psy.weak, Skills.bless.weak, Skills.curse.weak, Skills.almighty.medium, Skills.healing.weak, Skills.forceApplyBrainwash))
  skillMenu.cameras.hideFromDefault().addToCamera(manager.fullCam)
  skillMenu.visible = false
  skillMenu.y += 128

  val backgroundSource = SoundSource(true, true)
  backgroundSource.setBuffer(Sounds.background.buffer)

  var currentFocus: MenuFocus = MenuFocus.Root
  
  override def start(): Unit =
    add(menu)
    add(skillMenu)
    backgroundSource.play()

  def editFocused(by: Int): Unit =
    currentFocus match
      case MenuFocus.Root => menu.editSelection(by)
      case MenuFocus.Skill => skillMenu.editSelection(by)

  override def update(): Unit =
    super.update()

    if (GayG.input.dPadUp.justPressed) {
      editFocused(-1)
    } else if (GayG.input.dPadDown.justPressed) {
      editFocused(1)
    } else if (GayG.input.confirm.justPressed) {
      // TODO: this is actually quite complicated and
      // requires a lot of shifting around stuff
      // For now to test tho, lets do this
      if (currentFocus == MenuFocus.Root && menu.selectedOption == 1) {
        currentFocus = MenuFocus.Skill
        skillMenu.visible = true
      }
    } else if (GayG.input.cancel.justPressed) {
      currentFocus match
        case MenuFocus.Root => ()
        case MenuFocus.Skill =>
          currentFocus = MenuFocus.Root
          skillMenu.visible = false
    }

  override def close(): Unit =
    backgroundSource.cleanup()
