package pso.parallel_simulation

import ActorMessages._

/**
 * Created by mmiliauskass on 26/01/2014.
 */

case class SimulationOptions(bLow: Double, bUp: Double, dimCount: Int, phiP: Double, phiG: Double,
                             particleCount: Int, maxIterations: Int, fitnessFunc: (List[Double]) => Double)

class Simulation(options: SimulationOptions) {


  val clock: ClockActor = new ClockActor(options)

  for (i <- 1 to options.particleCount) {

    clock.add(new ParticleActor(
      options,
      clock
    ))

  }

  def start() {
    clock.start()
    clock ! Start
  }






}

