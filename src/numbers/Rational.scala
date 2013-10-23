/*
 * @author Robby McKilliam
 */

package numbers

import numbers.EuclideanDomain.gcd

object Rational {
  val one = Rational(1,1)
  val zero =  Rational(0,1)
  
  def apply(n : Integer, d : Integer) : Rational = {
    if(d==Integer.zero) throw new RuntimeException("Demoninator in rational is zero!")
    val g = gcd(n,d)
    if(d < Integer.zero) return new Rational(-n/g,-d/g) //denominator is always positive
    else return new Rational(n/g,d/g)
  }
  def apply(n : Int, d : Int) : Rational = Rational(Integer(n),Integer(d))
  def apply(n : Long, d : Long) : Rational = Rational(Integer(n),Integer(d))
}

/** Infinite precision rational number.  Will grow until your computer runs out of memory. */
protected class Rational(val n : Integer, val d: Integer) extends Field[Rational,Rational] with Ordered[Rational] {
  
  final def zero = Rational.zero
  final def one = Rational.one
  
  final def +(that : Rational) : Rational = Rational(that.d*n + that.n*d,that.d*d)
  final def unary_- : Rational = Rational(-n,d) //n- is the negative of n
  final def *(that : Rational) : Rational = Rational(that.n*n,that.d*d)
  final def /(that: Rational) : Rational = Rational(that.d*n,that.n*d)
  
  final def norm : Rational = Rational(n.norm,d) //same as absolute value
  
  final override def ==(that : Rational) : Boolean = that.n==n && that.d==d
  
  /** Uses scala's internal Ordered, only need to override compare */
  final def compare(that : Rational) : Int = {
    val num = (this-that).n
    if(num < Integer.zero) return -1
    else if(num > Integer.zero) return 1
    else return 0
  }
  
  final override def toString : String  = n.toString + "/" + d.toString
  
}
