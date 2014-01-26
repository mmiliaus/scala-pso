package pso

import scala.actors._
import support.Vector._

/**
 * Created by mmiliauskass on 24/01/2014.
 */

case class SimulationOptions(bLow: Double, bUp: Double, dimCount: Int, phiP: Double, phiG: Double,
                             particleCount: Int, maxIterations: Int, fitnessFunc: (List[Double]) => Double)

class PsoSimulation(options: SimulationOptions) {

  case class Ping(time: Int, gBestPosition: List[Double])
  case class Pong(pBestPosition: List[Double], from: Particle)
  case object Stop
  case object Start


  val clock: Clock = new Clock(options.maxIterations)

  for (i <- 1 to options.particleCount) {

    clock.add(new Particle(
      options,
      clock
    ))

  }

  def start() = clock ! Start




  class Clock(maxTicks: Int) extends Actor {

    private var busyParticles: Set[Particle] = Set.empty
    private var allParticles: List[Particle] = List()
    private var currentTime = 0

    private var gBestPosition: List[Double] = List()
    private var gBestFitness: Double = 0.0

    private var running = false

    start()


    def act() {
      loop {
        if (running && busyParticles.isEmpty)
          advance()

        reactToOneMessage()
      }
    }

    def reactToOneMessage() {
      react {
        case Pong(pBestPosition, particle) =>

//          println("*** ["+ pBestPosition.mkString(", ") + "] -> " + options.fitnessFunc(pBestPosition))

          if (options.fitnessFunc(pBestPosition) < gBestFitness) {
            gBestPosition = pBestPosition
            gBestFitness = options.fitnessFunc(pBestPosition)
          }
          assert(busyParticles contains particle)
          busyParticles -= particle
        case Stop =>
          for (p <- allParticles)
            p ! Stop
          exit()
        case Start =>
          gBestPosition = allParticles.reduceLeft(
            (minParticle, particle) =>
              if (options.fitnessFunc(particle.getCurrentPosition()) <
                options.fitnessFunc(minParticle.getCurrentPosition()))
                particle
              else
                minParticle
          ).getCurrentPosition()
          gBestFitness = options.fitnessFunc(gBestPosition)
          running = true
      }
    }

    def advance() {
      println("Best position: [" + gBestPosition.mkString(", ") + "] -> " + gBestFitness)

      if (busyParticles.isEmpty && currentTime == maxTicks) {
        println("** Reached max iterations limit: " + maxTicks + " **")
        this ! Stop
        return
      }

      currentTime += 1
      println("Advancing to time: " + currentTime)

      assert(busyParticles.isEmpty)
      for (p <- allParticles) {
        p ! Ping(currentTime, gBestPosition)
        busyParticles += p
      }


    }

    def add(p: Particle) {
      allParticles = p :: allParticles
    }


  }

  class Particle(simOptions: SimulationOptions, clock: Clock) extends Actor {

    private var currentPosition = getRandPosition()
    private var pBestPosition = currentPosition
    private var currentVelocity = getRandVelocity()

    private val vMax = math.abs(options.bUp - options.bLow)

    start()

    def act() {
      loop {
        react {
          case Stop => exit()
          case Ping(time, gBestPosition) => {
            if (!gBestPosition.isEmpty) {
              updateVelocity(time, gBestPosition)
              updatePosition()
              updatePersonalBest()
            }
//            println("*** ["+ currentPosition.mkString(", ") + "] -> " + options.fitnessFunc(currentPosition))
            clock ! Pong(pBestPosition, this)
          }
        }
      }
    }


    def getRandPosition(): List[Double] = {
      normalize(
        options.bLow,
        options.bUp,
        List.fill(options.dimCount)(math.random)
      )
    }

    def getRandVelocity(): List[Double] =
      normalize(
        -vMax,
        vMax,
        List.fill(options.dimCount)(math.random)
      )

    def getCurrentPosition() = currentPosition

    private def normalize(lo: Double, up: Double, xs: List[Double]) =
      xs map (x => (up - lo) * x + lo)

    private def inertiaWeight(time: Int): Double =
      options.bUp - (options.bUp - options.bLow) * (time / options.maxIterations)

    private def updateVelocity(time: Int, gBestPosition: List[Double]) = {
      val rP = math.random
      val rG = math.random

      currentVelocity = add(
        multi(currentVelocity, inertiaWeight(time)),
        add(
          multi(subtract(pBestPosition,currentPosition), rP * options.phiP),
          multi(subtract(gBestPosition,currentPosition), rG * options.phiG)
        )
      )

      currentVelocity = currentVelocity map (x =>
        if (x > vMax) vMax
        else if (x < -vMax) -vMax
        else x
      )
    }

    private def updatePosition() = {
      currentPosition = add(currentPosition, currentVelocity)
    }

    private def updatePersonalBest() =
      if (options.fitnessFunc(currentPosition) < options.fitnessFunc(pBestPosition))
          pBestPosition = currentPosition
  }


}