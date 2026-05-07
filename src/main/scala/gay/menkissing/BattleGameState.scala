package gay.menkissing

import gay.menkissing.battle.{BattleMenu, RootBattleMenu, Skill, SkillBattleMenu, Skills}
import gay.menkissing.engine.{GameManager, GayG, GayState, GayText}
import gay.menkissing.engine.audio.SoundSource
import gay.menkissing.battle.PercentBar
import gay.menkissing.battle.BattlePortrait
import gay.menkissing.battle.BasicFighter
import gay.menkissing.battle.Stats
import gay.menkissing.battle.Element
import gay.menkissing.battle.ResistElementMap
import gay.menkissing.battle.ResistLevel
import gay.menkissing.battle.PotentialElementMap
import gay.menkissing.battle.SkillPotential
import gay.menkissing.battle.Enemy

import scala.collection.mutable
import gay.menkissing.battle.Fighter
import gay.menkissing.battle.SkillTarget
import gay.menkissing.battle.EnemySelector
import gay.menkissing.common.math.PointF

enum MenuFocus {
  case Root
  case Skill
  case EnemySelectionOne
  case EnemySelectionAll
  case Hidden
}

class BattleGameState(manager: GameManager) extends GayState(manager):
  val menu =RootBattleMenu()
  menu.cameras.hideFromDefault().addToCamera(manager.fullCam)
  val skillMenu = SkillBattleMenu(menu)
  skillMenu.cameras.hideFromDefault().addToCamera(manager.fullCam)
  skillMenu.visible = false
  skillMenu.y += 128

  val mcFighter =
    BasicFighter(
      "luca",
      100,
      100,
      5,
      Stats(10, 10, 10, 10, 10),
      List(Skills.slash.powerSlash, Skills.ice.weak),
      Element.Slash,
      60,
      resistances = ResistElementMap(ice = ResistLevel.Resist, fire = ResistLevel.Weak),
      skillPotentials = PotentialElementMap(ice = SkillPotential.Up1, slash = SkillPotential.Up1, fire = SkillPotential.Down1)
      )

  val portrait = BattlePortrait(mcFighter, Textures.preload.lucaPortrait)
  portrait.cameras.hideFromDefault().addToCamera(manager.fullCam)
  portrait.y = draw.fullRenderHeight - portrait.graphicalHeight

  val enemyFighter =
    BasicFighter(
      "enemy",
      100,
      100,
      5,
      Stats(10, 10, 10, 10, 10),
      List(Skills.slash.powerSlash, Skills.electric.weak),
      Element.Strike,
      60,
      resistances = ResistElementMap(ice = ResistLevel.Weak)
    )

  val enemy = Enemy(enemyFighter, Textures.preload.testEnemy)
  enemy.x = 20
  enemy.y = 20

  var selectionQueued: Skill = null
  val enemies = mutable.ArrayBuffer(enemy)
  var selectedEnemy: Int = 0
  //val backgroundSource = SoundSource(true, true)
  //backgroundSource.setBuffer(Sounds.background.buffer)
  //backgroundSource.setGain(AudioTuning.maximumBGMMix * AudioTuning.settingsBGMMix)

  var currentFocus: MenuFocus = MenuFocus.Root

  var currentPlayer: Fighter = null

  val enemySelector = EnemySelector()
  enemySelector.cameras.hideFromDefault().addToCamera(manager.fullCam)
  enemySelector.visible = false


  
  def startPlayerTurn(portrait: BattlePortrait): Unit =
    menu.visible = true
    skillMenu.visible = false
    skillMenu.loadSkills(portrait.fighter.skills)
    currentPlayer = portrait.fighter

  override def start(): Unit =
    add(menu)
    add(skillMenu)
    add(portrait)
    add(enemy)
    add(enemySelector)
    startPlayerTurn(portrait)
    // backgroundSource.play()

  def editFocusedHorz(by: Int): Unit =
    currentFocus match
      case MenuFocus.EnemySelectionOne =>
        selectedEnemy += by
        selectedEnemy = math.floorMod(selectedEnemy, enemies.length)
        enemySelector.updateSelection(enemies(selectedEnemy))
      case _ => ()

  def editFocusedVert(by: Int): Unit =
    currentFocus match
      case MenuFocus.Root => menu.editSelection(by)
      case MenuFocus.Skill => skillMenu.editSelection(by)
      case _ => ()

  def startEnemySelect(multi: Boolean): Unit =
    menu.visible = false
    skillMenu.visible = false
    enemySelector.visible = true
    selectedEnemy = 0
    enemySelector.updateSelection(enemies.head)
    if (multi)
      currentFocus = MenuFocus.EnemySelectionAll
    else
      currentFocus = MenuFocus.EnemySelectionOne

  override def update(): Unit =
    super.update()

    if (GayG.input.dPadUp.justPressed) {
      editFocusedVert(-1)
    } else if (GayG.input.dPadDown.justPressed) {
      editFocusedVert(1)
    } else if (GayG.input.dPadLeft.justPressed) {
      editFocusedHorz(-1)
    } else if (GayG.input.dPadRight.justPressed) {
      editFocusedHorz(1)
    } else if (GayG.input.confirm.justPressed) {
      // TODO: this is actually quite complicated and
      // requires a lot of shifting around stuff
      // For now to test tho, lets do this
      currentFocus match
        case MenuFocus.Root => 
          menu.selectedOption match
            case RootBattleMenu.Options.Attack =>
              selectionQueued = currentPlayer.basicAttackSkill
              startEnemySelect(multi = false)
        case MenuFocus.Skill =>
          val skill = skillMenu.currentSelected.skill.get
          selectionQueued = skill
          skill.target match
            case SkillTarget.Self => ???
            case SkillTarget.Ally => ???
            case SkillTarget.AllAllies => ???
            case SkillTarget.Foe => startEnemySelect(multi = false)
            case SkillTarget.AllFoes => startEnemySelect(multi = true)
            case SkillTarget.All => ???
          
        case MenuFocus.EnemySelectionOne =>
          val selection = enemies(selectedEnemy)
          selectionQueued.performOne(currentPlayer, selection.fighter)(using GayG.random)
          enemySelector.updateSelection(selection)
          
        case MenuFocus.EnemySelectionAll => ()
        case MenuFocus.Hidden => ()
      
    } else if (GayG.input.cancel.justPressed) {
      currentFocus match
        case MenuFocus.Root => ()
        case MenuFocus.Skill =>
          currentFocus = MenuFocus.Root
          skillMenu.visible = false
        case MenuFocus.EnemySelectionOne => ()
        case MenuFocus.EnemySelectionAll => ()
        case MenuFocus.Hidden => ()
    }

  override def close(): Unit =
    //backgroundSource.cleanup()
    ()
