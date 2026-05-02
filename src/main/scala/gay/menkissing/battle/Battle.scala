package gay.menkissing
package battle

import collection.mutable


// Due to us NOT being fully in control of timing, we're stuck having to use
// a state machine
//
enum BattleState {
  case Wait
  case TakeAction
  case PerformAction
}


final case class SidedFighter(allied: Boolean, fighter: Fighter)

final case class QueuedAction(target: Either[SidedFighter, List[SidedFighter]], skill: Skill)

final case class Events(queue: mutable.Buffer[BattleEvent])

enum BattleEvent {
  case GotUp(target: SidedFighter)
  case Downed(target: SidedFighter)
  case OneMore(cause: SidedFighter)
  case UpdateHP(target: SidedFighter)
  case UpdateSP(target: SidedFighter)
  case BattleLoss
  case BattleWin
}



final class Battle(val gameState: BattleGameState) extends common.PRNG {
  var state: BattleState = BattleState.Wait
  var allies: List[SidedFighter] = List()
  var enemies: List[SidedFighter] = List()
  var turnOrder: Vector[SidedFighter] = Vector()
  val loadedActions = mutable.Buffer[QueuedAction]()
  var turnIdx: Int = 0
  var oneMore: Boolean = false
  def start(inAllies: List[Fighter], inEnemies: List[Fighter]): Unit = {
    state = BattleState.Wait
    allies = inAllies.map(SidedFighter(true, _))
    enemies = inEnemies.map(SidedFighter(false, _))
    setupBattle()
  }
  def setupBattle(): Unit = {
    turnOrder = allies.concat(enemies).toVector
    turnIdx = 0

    println("encounter start")
    printStatus()
  }
  def printStatus(): Unit = {
    allies.foreach(_.fighter.printInfo())
    enemies.foreach(_.fighter.printInfo())
  }

  def willBePlayerTurn: Boolean =
    turnOrder(turnIdx).allied

  def update(): Unit = {
    state match
      case BattleState.Wait =>
        if (loadedActions.nonEmpty)
          state = BattleState.TakeAction
      case BattleState.TakeAction =>

      case BattleState.PerformAction => ()
  }

  def advanceTurnOrder(): Unit =
    if (!oneMore) {
      turnIdx += 1
      turnIdx %= turnOrder.length
      while (turnOrder(turnIdx)._2.health <= 0) {
        turnIdx += 1
        turnIdx %= turnOrder.length
      }
    }
    oneMore = false


  def chooseSkill(fighter: Fighter): Skill = {
    val skills = fighter.skills
    val skillN = skills.length
    for ((skill, i) <- skills.zipWithIndex) {
      println(s"${i + 1}: ${skill.displayStr(fighter)}")
    }
    var selectedSkill = -1
    while (selectedSkill < 0 || selectedSkill >= skillN) {
      selectedSkill = io.StdIn.readInt() - 1
      if (selectedSkill > 0 && selectedSkill < skillN) {
        if (!skills(selectedSkill).canAffordSkill(fighter)) {
          selectedSkill = -1
        }
      }

    }
    skills(selectedSkill)
  }
  def chooseFighterIn(from: List[SidedFighter]): SidedFighter = {
    from.zipWithIndex.foreach { case (SidedFighter(_, fighter), i) =>
      println(s"${i + 1}: ${fighter.infoStr}")
    }
    val fromLen = from.length
    var selected = -1
    while (selected < 0 || selected >= fromLen) {
      selected = io.StdIn.readInt() - 1
    }
    from(selected)
  }
  def processSkill(skill: Skill, caster: SidedFighter, target: SidedFighter)(using Events): Unit = {
    skill.processSkillCost(caster.fighter, caster.allied)
    val res = skill.run(this, caster, target)
    if (res.wasKnockedDown) {
      processOneMore(caster)
    }
  }
  def selectTargetRandom(allySide: Boolean): SidedFighter = {
    if (allySide) {
      randomFrom(allies.filter(_.fighter.health > 0).toArray)
    } else {
      randomFrom(enemies.toArray)
    }
  }
  def selectSide(allySide: Boolean): List[SidedFighter] = {
    if (allySide) {
      allies
    } else {
      enemies
    }
  }

  def queueAction(action: QueuedAction): Unit =
    loadedActions.append(action)

  // utility for classes calling back in
  def runAction(action: QueuedAction, caster: SidedFighter)(using Events): Unit = {
    action.target match {
      case Left(value) => processSkill(action.skill, caster, value)
      case Right(value) => processMultiSkill(action.skill, caster, value)
    }
  }
  def processOneMore(fighter: SidedFighter)(using ev: Events): Unit = {
    if (oneMore) return
    oneMore = true
    ev.queue.append(BattleEvent.OneMore(fighter))
  }
  def processKill(target: SidedFighter): Unit = {
    if (target.allied) {
      println(s"${target.fighter.name} was defeated...")
      if (!allies.exists(_.fighter.health > 0)) {
        println("The party was defeated...")
        state = BattleState.Wait
      }
    } else {
      println(s"${target.fighter.name} was defeated!")
      enemies = enemies.filterNot(_ == target)
      if (enemies.isEmpty) {
        println("All enemies defeated!")
        state = BattleState.Wait
      }
    }
  }
  def processMultiSkill(skill: Skill, caster: SidedFighter, targets: List[SidedFighter])(using Events): Unit = {
    skill.processSkillCost(caster.fighter, caster.allied)
    var canOneMore: Boolean = targets.nonEmpty
    skill.runAll(this, caster, targets).zip(targets).foreach { case (result, fighter) =>
      // Like in vanilla p3, all enemies must be knocked down to get a 1 more
      canOneMore &&= result.wasKnockedDown
    }
    if (canOneMore) {
      processOneMore(caster)
    }
  }
  def playerTurn(player: SidedFighter)(using Events): Unit = {
    val fighter = player.fighter
    println(s"player turn: ${fighter.infoStr}")
    if (fighter.curAilment.forall(_.actionable)) {
      val skill = chooseSkill(fighter)
      val action = skill.target match {
        case SkillTarget.Self => {
          QueuedAction(Left(player), skill)
        }
        case SkillTarget.Ally => {
          val ally = chooseFighterIn(allies.filter(it => skill.validTarget(this, it.fighter)))
          QueuedAction(Left(ally), skill)
        }
        case SkillTarget.AllAllies => {
          QueuedAction(Right(allies), skill)
        }
        case SkillTarget.Foe => {
          val foe = chooseFighterIn(enemies.filter(it => skill.validTarget(this, it.fighter)))
          QueuedAction(Left(foe), skill)
        }
        case SkillTarget.AllFoes => {
          QueuedAction(Right(enemies), skill)
        }
        case SkillTarget.All => {
          QueuedAction(Right(turnOrder.toList), skill)
        }
      }
      fighter.curAilment match
        case Some(value) => value.withAction(this, Some(action), player)
        case None => this.runAction(action, player)
    } else {
      fighter.curAilment.get.withAction(this, None, player)
    }


  }
  def enemyTurn(enemy: SidedFighter)(using Events): Unit = {
    val availableSkills = enemy.fighter.skills.filter(it => it.canAffordSkill(enemy.fighter, false) && enemy.fighter.curAilment.forall(_.canPerformAction(this, it)))
    val skill = randomFrom(availableSkills.toArray)
    val action = skill.target match {
      case SkillTarget.Self => {
        QueuedAction(Left(enemy), skill)
      }
      case SkillTarget.Ally => {
        val possibleTargets = enemies.filter(it => skill.validTarget(this, it.fighter))
        val ally = possibleTargets(math.floor(math.random() * possibleTargets.length).toInt)
        QueuedAction(Left(ally), skill)
      }
      case SkillTarget.AllAllies => {
        QueuedAction(Right(enemies), skill)
      }
      case SkillTarget.Foe => {
        val possibleTargets = allies.filter(it => skill.validTarget(this, it.fighter))
        val target = possibleTargets(math.floor(math.random() * possibleTargets.length).toInt)
        QueuedAction(Left(target), skill)
      }
      case SkillTarget.AllFoes => {
        QueuedAction(Right(allies), skill)
      }
      case SkillTarget.All => {
        QueuedAction(Right(turnOrder.toList), skill)
      }
    }
    enemy.fighter.curAilment match {
      case Some(value) => value.withAction(this, Some(action), enemy)
      case None => this.runAction(action, enemy)
    }


  }
  def resultWin(): Unit = {
    println("u won!")
  }
  def resultLose(): Unit = {
    println("u lost")
  }
  def random(): Double = {
    math.random()
  }
}


class BattleHUD {
  var nameText: String = ""
  var hpAmount: Int = 0
  var hpMax: Int = 1
  // : (
  def setHUD(fighter: Fighter): Unit = {
    nameText = fighter.name
    hpAmount = fighter.health
    hpMax = fighter.maxHP
  }

  def printHUD(): Unit = {
    println(s"${nameText} ${hpAmount}/${hpMax}")
  }
}