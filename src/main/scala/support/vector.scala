package support

/**
 * Created by mmiliauskass on 24/01/2014.
 */
object Vector {


  def add(v1: List[Double], v2: List[Double]): List[Double] = dotAction(v1,v2)((x,y)=>x + y)

  def subtract(v1: List[Double], v2: List[Double]): List[Double] = dotAction(v1,v2)((x,y)=>x - y)

  def multi(v1: List[Double], scalar: Double) = v1 map {x => x * scalar}

  def dotAction(v1: List[Double], v2: List[Double])(action:(Double, Double) => Double): List[Double] =
    v1.zip(v2).map {case (x,y) => action(x,y)}

}
