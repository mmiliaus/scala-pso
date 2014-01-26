package pso.parallel_simulation

/**
 * Created by mmiliauskass on 26/01/2014.
 */
object ActorMessages {

  case class Ping(time: Int, gBestPosition: List[Double])
  case class Pong(pBestPosition: List[Double], from: ParticleActor)
  case object Stop
  case object Start

}
