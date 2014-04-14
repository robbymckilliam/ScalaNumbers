/*
 * @author Robby McKilliam
 */

package numbers

import scala.math.max

trait Polynomial {
  val order : Int
}

/** Polynomial ring with coefficients taken from elements of a ring */
class PolynomialRing[R <: Ring[R]](val c : Seq[R]) extends Polynomial with Ring[PolynomialRing[R]] {
  
  override val order = c.length
  
  /** The operation */
  override def +(that : PolynomialRing[R]) : PolynomialRing[R] = {
    val o = max(order,that.order)
    throw new UnsupportedOperationException("No + at the moment")
  }
  /** Polynomial multiply (convolution) */
  override def *(that : PolynomialRing[R]) : PolynomialRing[R] = throw new UnsupportedOperationException("No * at the moment")
  /** Polynomial additive inverse */
  override def unary_- : PolynomialRing[R] = throw new UnsupportedOperationException("No - at the moment")
  /** The identity element*/
  override def zero : PolynomialRing[R] = throw new UnsupportedOperationException("No zero at the moment")
  /** Test for equality */
  override def ==(that : PolynomialRing[R]) : Boolean = throw new UnsupportedOperationException("No == at the moment")
  
}