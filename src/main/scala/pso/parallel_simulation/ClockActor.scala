package pso.parallel_simulation

import scala.actors._
import ActorMessages._

/**
 * Created by mmiliauskass on 26/01/2014.
 */

class ClockActor(simOptions: SimulationOptions) extends Actor {

  private var busyParticleActors: Set[ParticleActor] = Set.empty
  private var allParticleActors: List[ParticleActor] = List()
  private var currentTime = 0

  private var gBestPosition: List[Double] = List()
  private var gBestFitness: Double = 0.0

  private var running = false


  def act() {
    loop {
      if (running && busyParticleActors.isEmpty)
        advance()

      reactToOneMessage()
    }
  }

  def reactToOneMessage() {
    react {
      case Pong(pBestPosition, particle) =>

        //          println("*** ["+ pBestPosition.mkString(", ") + "] -> " + simOptions.fitnessFunc(pBestPosition))

        if (simOptions.fitnessFunc(pBestPosition) < gBestFitness) {
          gBestPosition = pBestPosition
          gBestFitness = simOptions.fitnessFunc(pBestPosition)
        }
        assert(busyParticleActors contains particle)
        busyParticleActors -= particle
      case Stop =>
        for (p <- allParticleActors)
          p ! Stop
        exit()
      case Start =>

        allParticleActors.foreach(_.start())

        gBestPosition = allParticleActors.reduceLeft(
          (minParticleActor, particle) =>
            if (simOptions.fitnessFunc(particle.getCurrentPosition()) <
              simOptions.fitnessFunc(minParticleActor.getCurrentPosition()))
              particle
            else
              minParticleActor
        ).getCurrentPosition()
        gBestFitness = simOptions.fitnessFunc(gBestPosition)
        running = true
    }
  }

  def advance() {
    println("Best position: [" + gBestPosition.mkString(", ") + "] -> " + gBestFitness)

    if (busyParticleActors.isEmpty && currentTime == simOptions.maxIterations) {
      println("** Reached max iterations limit: " + simOptions.maxIterations + " **")
      this ! Stop
      return
    }

    currentTime += 1
    println("Advancing to time: " + currentTime)

    assert(busyParticleActors.isEmpty)
    for (p <- allParticleActors) {
      p ! Ping(currentTime, gBestPosition)
      busyParticleActors += p
    }


  }

  def add(p: ParticleActor) {
    allParticleActors = p :: allParticleActors
  }


}
