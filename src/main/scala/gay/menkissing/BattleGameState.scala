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
import gay.menkissing.engine.anim.GayTween
import gay.menkissing.engine.anim.TweenAccessor

enum MenuFocus {
  case Root
  case Skill
  case EnemySelectionOne
  case EnemySelectionAll
  case Hidden
}

class BattleGameState() extends GayState():
  val menu =RootBattleMenu()
  menu.setLayer(GayG.initialCamera)
  val skillMenu = SkillBattleMenu(menu)
  skillMenu.setLayer(GayG.initialCamera)
  skillMenu.visible = false
  skillMenu.y += 128

  val mcFighter =
    BasicFighter(
      "luca",
      100,
      100,
      5,
      Stats(10, 10, 10, 10, 10),
      List(Skills.slash.powerSlash, Skills.ice.weak, Skills.ice.bloody),
      Element.Slash,
      60,
      resistances = ResistElementMap(
        Element.Fire -> ResistLevel.Weak,
        Element.Ice -> ResistLevel.Resist
      ),
      skillPotentials = PotentialElementMap(
        Element.Fire -> SkillPotential.Down1,
        Element.Ice -> SkillPotential.Up1,
        Element.Slash -> SkillPotential.Up1
      )
      )

  val portrait = BattlePortrait(mcFighter, Textures.preload.lucaPortrait)
  portrait.setLayer(GayG.initialCamera)
  portrait.y = GayG.initialCamera.worldSize.y - portrait.graphicalHeight

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
      resistances = ResistElementMap(Element.Ice -> ResistLevel.Weak)
    )

  val enemy = Enemy(enemyFighter, Textures.preload.testEnemy)
  enemy.setLayer(GayG.initialCamera)
  enemy.x = 100
  enemy.y = 100
  

  var selectionQueued: Skill = null
  val enemies = mutable.ArrayBuffer(enemy)
  var selectedEnemy: Int = 0
  //val backgroundSource = SoundSource(true, true)
  //backgroundSource.setBuffer(Sounds.background.buffer)
  //backgroundSource.setGain(AudioTuning.maximumBGMMix * AudioTuning.settingsBGMMix)

  var currentFocus: MenuFocus = MenuFocus.Root

  var currentPlayer: Fighter = null




  
  def startPlayerTurn(portrait: BattlePortrait): Unit =
    menu.visible = true
    skillMenu.visible = false
    skillMenu.loadSkills(portrait.fighter.skills)
    currentPlayer = portrait.fighter

  override def start(): Unit =
    add(enemy)
    add(menu)
    add(skillMenu)
    add(portrait)
    
    startPlayerTurn(portrait)

    // MUST be in here otherwise it gets reset
    // backgroundSource.play()


  def cancelEnemySelection(): Unit =
    enemies.foreach(_.selector.visible = false)

  def updateEnemySelection(): Unit =
    cancelEnemySelection()
    enemies(selectedEnemy).selector.visible = true

  def editFocusedHorz(by: Int): Unit =
    currentFocus match
      case MenuFocus.EnemySelectionOne =>
        selectedEnemy += by
        selectedEnemy = math.floorMod(selectedEnemy, enemies.length)
        updateEnemySelection()
      case _ => ()

  def editFocusedVert(by: Int): Unit =
    currentFocus match
      case MenuFocus.Root => menu.editSelection(by)
      case MenuFocus.Skill => skillMenu.editSelection(by)
      case _ => ()

  def startEnemySelect(multi: Boolean): Unit =
    menu.visible = false
    skillMenu.visible = false
    selectedEnemy = 0
    updateEnemySelection()
    if (multi)
      currentFocus = MenuFocus.EnemySelectionAll
    else
      currentFocus = MenuFocus.EnemySelectionOne

  def switchFocus(focus: MenuFocus): Unit =
    currentFocus = focus
    focus match
      case MenuFocus.Root =>
        skillMenu.visible = false
        menu.visible = true
      case MenuFocus.Skill =>
        skillMenu.visible = true
        menu.visible = true
      case MenuFocus.EnemySelectionOne =>
        skillMenu.visible = false
        menu.visible = false
      case MenuFocus.EnemySelectionAll =>
        skillMenu.visible = false
        menu.visible = false
      case MenuFocus.Hidden =>
        skillMenu.visible = false
        menu.visible = false
    

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
            case RootBattleMenu.Options.Skill =>
              skillMenu.visible = true
              currentFocus = MenuFocus.Skill
            case RootBattleMenu.Options.Item => ()
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
          
        case MenuFocus.EnemySelectionAll => ()
        case MenuFocus.Hidden => ()
      
    } else if (GayG.input.cancel.justPressed) {
      currentFocus match
        case MenuFocus.Root => ()
        case MenuFocus.Skill =>
          currentFocus = MenuFocus.Root
          skillMenu.visible = false
        case MenuFocus.EnemySelectionOne =>
          cancelEnemySelection()
          if (selectionQueued.personaSkill) {
            switchFocus(MenuFocus.Skill)
          } else {
            switchFocus(MenuFocus.Root)
          }
        case MenuFocus.EnemySelectionAll => ()
        case MenuFocus.Hidden => ()
    }

  override def destroy(): Unit =
    super.destroy()
    //backgroundSource.cleanup()
    ()
