package pso.parallel_simulation

import scala.actors._
import ActorMessages._
import support.Vector._

/**
 * Created by mmiliauskass on 26/01/2014.
 */
class ParticleActor(simOptions: SimulationOptions, clock: ClockActor) extends Actor {

  private var currentPosition = getRandPosition()
  private var pBestPosition = currentPosition
  private var currentVelocity = getRandVelocity()

  private val vMax = math.abs(simOptions.bUp - simOptions.bLow)

  def act(): Unit = loop {
    react {
      case Stop => exit()
      case Ping(time, gBestPosition) => {
        if (!gBestPosition.isEmpty) {
          updateVelocity(time, gBestPosition)
          updatePosition()
          updatePersonalBest()
        }
        clock ! Pong(pBestPosition, this)
      }
    }
  }

  def getRandPosition(): List[Double] =
    normalize(
      simOptions.bLow,
      simOptions.bUp,
      List.fill(simOptions.dimCount)(math.random)
    )

  def getRandVelocity(): List[Double] =
    normalize(
      -vMax,
      vMax,
      List.fill(simOptions.dimCount)(math.random)
    )

  def getCurrentPosition():List[Double] = currentPosition

  private def normalize(lo: Double, up: Double, xs: List[Double]): List[Double] =
    xs map (x => (up - lo) * x + lo)

  private def inertiaWeight(time: Int): Double =
    simOptions.bUp - (simOptions.bUp - simOptions.bLow) * (time / simOptions.maxIterations)

  private def updateVelocity(time: Int, gBestPosition: List[Double]):Unit = {
    val rP = math.random
    val rG = math.random

    currentVelocity = add(
      multi(currentVelocity, inertiaWeight(time)),
      add(
        multi(subtract(pBestPosition,currentPosition), rP * simOptions.phiP),
        multi(subtract(gBestPosition,currentPosition), rG * simOptions.phiG)
      )
    )

    currentVelocity = currentVelocity map ((x:Double) =>
      if (x > vMax) vMax
      else if (x < -vMax) -vMax
      else x
    )
  }

  private def updatePosition():Unit =
    currentPosition = add(currentPosition, currentVelocity)

  private def updatePersonalBest():Unit =
    if (simOptions.fitnessFunc(currentPosition) < simOptions.fitnessFunc(pBestPosition))
      pBestPosition = currentPosition
}

