// Example Bot #1: The Reference Bot

import XY._
import View._
import CommandParser._
import BotImpl._


// TODO: prevent shadowing another bot (but it gives me a lot of points) -> or the other way around
// TODO: prevent getting stuck in obstacles
// TODO: kill agressive animals if too many and too close
// TODO: orfan missiles should reintegrate to parent
/** This bot builds a 'direction value map' that assigns an attractiveness score to
  * each of the eight available 45-degree directions. Additional behaviors:
  * - aggressive missiles: approach an enemy master, then explode
  * - defensive missiles: approach an enemy slave and annihilate it
  *
  * The master bot uses the following state parameters:
  *  - dontFireAggressiveMissileUntil
  *  - dontFireDefensiveMissileUntil
  *  - lastDirection
  * The mini-bots use the following state parameters:
  *  - mood = Aggressive | Defensive | Lurking
  *  - target = remaining offset to target location
  */
object ControlFunction
{
  def forMaster(bot: Bot) {

    val (directionValue, nearestEnemyMaster, nearestEnemySlave) = analyzeViewAsMaster(bot.view)

    val dontFireAggressiveMissileUntil = bot.inputAsIntOrElse("dontFireAggressiveMissileUntil", -1)
    val dontFireDefensiveMissileUntil = bot.inputAsIntOrElse("dontFireDefensiveMissileUntil", -1)
    val lastDirection45 = bot.inputAsIntOrElse("lastDirection", 0)

    moveMaster

    if(inConditionToAttack) {
      nearestEnemyMaster match {
        case None =>
        case Some(enemyMasterRelativePosition) =>
          val (slaveDirection, newEnemyRelativePosition) = calculateCoordinates(enemyMasterRelativePosition)
          bot.spawn(slaveDirection, "mood" -> "Aggressive", "target" -> newEnemyRelativePosition)
          bot.set("dontFireAggressiveMissileUntil" -> (bot.time + enemyMasterRelativePosition.stepCount + 1))
      }
    }
    else if(inConditionsToDefend) {
      nearestEnemySlave match {
        case None =>
        case Some(enemySlaveRelativePosition) =>
          if(enemySlaveTooClose(enemySlaveRelativePosition)) {
            val (slaveDirection, newEnemyRealtivePosition) = calculateCoordinates(enemySlaveRelativePosition)
            bot.spawn(slaveDirection, "mood" -> "Defensive", "target" -> newEnemyRealtivePosition)
            bot.set("dontFireDefensiveMissileUntil" -> (bot.time + enemySlaveRelativePosition.stepCount + 1))
          }
      }
    }

    // determine movement direction
    def moveMaster {
      // TODO: change this to prevent blocking loops
      directionValue(lastDirection45) += 10 // try to break ties by favoring the last direction
      //directionValue(XY.rotateClockwise(lastDirection45)) += 10 // try to break ties by favoring a turn
      //directionValue(XY.randomDirection) += 10 // try to break ties by favoring a random new dir

      val bestDirection45 = directionValue.zipWithIndex.maxBy(_._1)._2
      val direction = XY.fromDirection45(bestDirection45)

      bot.move(direction)
      bot.set("lastDirection" -> bestDirection45)
    }

    def inConditionToAttack: Boolean = {
      (dontFireAggressiveMissileUntil < bot.time) && (bot.energy > 100)
    }

    def inConditionsToDefend: Boolean = {
      dontFireDefensiveMissileUntil < bot.time && bot.energy > 100
    }

    def calculateCoordinates(enemyMasterRelativePosition: XY): (XY, XY) = {
      val slaveDirection = enemyMasterRelativePosition.signum
      val newEnemyRealtivePosition = enemyMasterRelativePosition - slaveDirection // we place slave nearer target, so subtract that from overall delta
      (slaveDirection, newEnemyRealtivePosition)
    }

    def enemySlaveTooClose(enemySlaveRelativePosition: XY): Boolean = {
      enemySlaveRelativePosition.stepCount < 8
    }


  }


  def forSlave(bot: MiniBot) {
    bot.inputOrElse("mood", "Lurking") match {
      case "Aggressive" => reactAsAggressiveMissile(bot)
      case "Defensive" => reactAsDefensiveMissile(bot)
      case s: String => bot.log("unknown mood: " + s)
    }
  }

  private def reactAsAggressiveMissile(bot: MiniBot) {
    // TODO: understand signum and delta
    bot.view.offsetToNearest('m') match {
      case Some(enemyMasterDistance: XY) =>
        // another master is visible at the given relative position (i.e. position delta)

        if(atBlowDistance(enemyMasterDistance)) {
          bot.explode(4)
        } else {
          moveCloser(bot, enemyMasterDistance)
        }
      case None =>
        followTargetingStrategy(bot)
    }
  }

  private def reactAsDefensiveMissile(bot: MiniBot) {
    bot.view.offsetToNearest('s') match {
      case Some(delta: XY) =>
        // another slave is visible at the given relative position (i.e. position delta)
        // TODO: try this change. Now the defensive missiles explode too. Should do it only if close to master
        if(atBlowDistance(delta) && closeToMaster(bot)) {
          bot.explode(2) // TODO: check if right radius
        } else {
          moveCloser(bot, delta)
        }

      case None =>
        followTargetingStrategy(bot)
    }
  }

  // TODO: finish this
  private def closeToMaster(bot: MiniBot): Boolean = {
    bot.view.offsetToNearest('M') match {
      case Some(delta: XY) =>
        if(delta.length <=3)
          true
        else
          false
      case None =>
        false
    }
  }

  private def atBlowDistance(delta: XY): Boolean = {
    delta.length <= 2
  }


  private def moveCloser(bot: MiniBot, delta: XY) {
    bot.move(delta.signum)
    bot.set("rx" -> delta.x, "ry" -> delta.y)
  }

  // no target visible -- follow our targeting strategy
  private def followTargetingStrategy(bot: MiniBot) {
    val target = bot.inputAsXYOrElse("target", XY.Zero)

    if (notAtTarget(target)) {
      keepFollowingTarget
    } else {
      switchPurpose
    }

    def notAtTarget(target: XY): Boolean = {
      target.isNonZero
    }

    def setNewTarget(bot: MiniBot, target:XY, unitDelta:XY){
      val remainder = target - unitDelta // e.g. = CellPos(-7,5)
      bot.set("target" -> remainder)
    }

    def switchPurpose: Bot = {
      // at target -- but we did not annihilate yet, and are not pursuing anything?!? => switch purpose
      bot.set("mood" -> "Lurking", "target" -> "")
      bot.say("Lurking")
    }

    def keepFollowingTarget {
      val unitDelta = target.signum // e.g. CellPos(-8,6) => CellPos(-1,1)
      bot.move(unitDelta)
      setNewTarget(bot, target, unitDelta)
    }
  }



  /** Analyze the view, building a map of attractiveness for the 45-degree directions and
    * recording other relevant data, such as the nearest elements of various kinds.
    */
  def analyzeViewAsMaster(view: View) = {
    val directionValue = Array.ofDim[Double](8)
    var nearestEnemyMaster: Option[XY] = None
    var nearestEnemySlave: Option[XY] = None

    val cells = view.cells
    val cellCount = cells.length

    def sideBySide(stepDistance: Int): Boolean = {
      stepDistance == 1
    }

    def twoStepsAway(stepDistance: Int): Boolean = {
      stepDistance == 2
    }

    def closeForABadBeast(stepDistance: Int): Boolean = {
      stepDistance < 4
    }

    // TODO: distance should ponderate
    for(i <- 0 until cellCount) {
      val cellRelPos = view.relPosFromIndex(i)
      if(cellRelPos.isNonZero) {
        val stepDistance = cellRelPos.stepCount
        val value: Double = cells(i) match {
            // TODO: change this perception
          case 'm' => // another master: not dangerous, but an obstacle
            nearestEnemyMaster = Some(cellRelPos)
            if(sideBySide(stepDistance))        -1000
            //else                                0
            else                                -900 / stepDistance
            // TODO: change this perception
          case 's' => // another slave: potentially dangerous?
            nearestEnemySlave = Some(cellRelPos)
                                                -800 / stepDistance

          case 'S' => // out own slave
                                                0

          case 'B' => // good beast: valuable, but runs away
            if(sideBySide(stepDistance))        600
            else if(twoStepsAway(stepDistance)) 300
            else                                (150 - stepDistance * 15).max(10)

          case 'P' => // good plant: less valuable, but does not run
            if(sideBySide(stepDistance))        500
            else if(twoStepsAway(stepDistance)) 250
            else                                (150 - stepDistance * 10).max(10)

          case 'b' => // bad beast: dangerous, but only if close
            if(closeForABadBeast(stepDistance)) -400 / stepDistance
            else                                -50 / stepDistance

          case 'p' => // bad plant: bad, but only if I step on it
            if(sideBySide(stepDistance))        -1100
            else                                0

          // TODO: harmless but should detract from direction to avoid going for protected elements
          case 'W' => // wall: harmless, just don't walk into it
            if(sideBySide(stepDistance))        -1000
            //else if(twoStepsAway(stepDistance)) -950
            else                                0

          case _ =>                             0
        }
        val direction45 = cellRelPos.toDirection45
        directionValue(direction45) += value
      }
    }
    (directionValue, nearestEnemyMaster, nearestEnemySlave)
  }
}



// -------------------------------------------------------------------------------------------------
// Framework
// -------------------------------------------------------------------------------------------------

class ControlFunctionFactory {
  def create = (input: String) => {
    val (opcode, params) = CommandParser(input)
    opcode match {
      case "React" =>
        val bot = new BotImpl(params)
        if( bot.generation == 0 ) {
          ControlFunction.forMaster(bot)
        } else {
          ControlFunction.forSlave(bot)
        }
        bot.toString
      case _ => "" // OK
    }
  }
}

