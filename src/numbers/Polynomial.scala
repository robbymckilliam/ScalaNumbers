/*
 * @author Robby McKilliam
 */

package numbers

import scala.math.max

trait Polynomial[E] {
  val order : Int
  def coefficient(n : Int) : E
}

object PolynomialRing {
  /** Polynomial with coefficent in from c */
  def apply[R <: Ring[R]](c : Seq[R]) : PolynomialRing[R] = new PolynomialRing[R](c)
  /** Polynomial of order zero with single nonzero coefficent c0 */
  def apply[R <: Ring[R]](c0 : R) : PolynomialRing[R] = PolynomialRing[R](IndexedSeq[R](c0))
}

/** Polynomial ring with coefficients taken from elements of a ring */
class PolynomialRing[R <: Ring[R]](val c : Seq[R]) extends Polynomial[R] with Ring[PolynomialRing[R]] {
  if(c.size == 0) throw new ArrayIndexOutOfBoundsException("Coefficient vector c must have size atleast 1. The zero Polynomial is constructed by making the element/s of c all zero.")
  
  protected val czero : R = c(0).zero //zero coefficient
  
  /** Order of the polynomial */
  override val order = {
    def f(i :Int) : Int = if(c(i) != czero) i else if(i==0) -1 else f(i-1)
    f(c.length-1)
  }
  
  override def coefficient(n : Int) : R = {
    if(order == -1) czero
    else if( n >= 0 && n <= order ) c(n)
    else czero
  }
  
  /** The operation */
  override def +(that : PolynomialRing[R]) : PolynomialRing[R] = {
    val neworder = max(order,that.order)
    val newc = (0 to neworder).map( n => coefficient(n) + that.coefficient(n) )
    return new PolynomialRing[R](newc)
  }
  
  /** Polynomial multiply (convolution) */
  override def *(that : PolynomialRing[R]) : PolynomialRing[R] = throw new UnsupportedOperationException("No * at the moment")
  /** Polynomial additive inverse */
  override def unary_- : PolynomialRing[R] = PolynomialRing[R](c.map(x => -x))
  /** The identity element*/
  override def zero : PolynomialRing[R] = PolynomialRing[R](czero)
  /** Test for equality */
  override def ==(that : PolynomialRing[R]) : Boolean = {
    if(order != that.order) return false
    return (0 to order).foldLeft(true)( (p, n) => coefficient(n) == that.coefficient(n))
  }
  
}