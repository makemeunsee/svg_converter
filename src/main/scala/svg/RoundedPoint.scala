package svg

/**
 * Created by markus on 02/11/2014.
 */
trait RoundedPoint {
  def x: Float
  def y: Float
}

object RoundedPoint {
  def apply(x: Float, y: Float): RoundedPoint = {
    SecretRoundedPoint(math.round(100*x)/100f, math.round(100*y)/100f)
  }
}

// case class for built in convenience methods. must provide already rounded x and y.
private case class SecretRoundedPoint( x: Float, y: Float ) extends RoundedPoint {
  override def toString = s"($x,$y)"
}
