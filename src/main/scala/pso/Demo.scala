package pso

/**
 * Created by mmiliauskass on 25/01/2014.
 */
object Demo extends App {

  val simOptions = SimulationOptions(
    bLow = -5.12,
    bUp = 5.12,
    dimCount = 2,
    phiP = 2.0,
    phiG = 2.0,
    particleCount = 1000,
    maxIterations = 100000,
    fitnessFunc = p => (p.map(x => -x * math.sin(math.sqrt(math.abs(x))))).reduceLeft(_+_)
  )

  val simulation = new PsoSimulation(simOptions)
  simulation.start()

}
