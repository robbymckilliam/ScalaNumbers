/*
 * Special functions, exp, gamma, factorial, etc.
 * @author Robby McKilliam 11/1/2015
 */

package numbers

import scala.annotation.tailrec
import RingWithUnity.pow

object SpecialFunctions {

  /// Rational precision defaults to 15 decimal places
  val RATIONAL_PRECISION = pow(Rational(1,10),15)
  
  ///Factorial of integer n
  def factorial(n : Integer) : Integer = {
    if(n<Integer.zero) throw new RuntimeException("Cannot compute factorial of negative number.")
    @tailrec def f(i : Integer, v : Integer) : Integer = if(i==Integer.zero) return v else return f(i-1,v*i)
    return f(n,Integer.one)
  }  
  
  /** 
   * Approximates the exponential function by Taylor series expansion.  This converges quickly if
   * x is not too big.  
   * 
   * @param x   The exponent. Will compute e^x
   * @param precision terms will be computed until less than precision decima places.  Result should be approximately this many decimal places accurate. Default 1e-15.
   */
  def exp_series(x : Rational, precision : Rational = RATIONAL_PRECISION) : Rational = {
    @tailrec def f(k : Int, term: Rational, v : Rational) : Rational = {
      val nextterm = term*x/k
      if(precision.normlarger(term)) return v + nextterm
      else return f(k+1,nextterm,v+nextterm)
    }
    return f(1,1,1)
  }
  
  /** 
   * Approximates the exponential function by continued fraction.  
   * 
   * 1 + 2x/(2 - x + x^2/(6 + x^2/(10 + x^2/(14 + x^2/... ))))
   * 
   * Hopefully reasonable accurate even when x is fairly large.
   * 
   * @param x   The exponent. Will compute e^x
   * @param precision terms will be computed until a the result is modified by less than precision decimal places.  Result should be approximately this many decimal places accurate. Default 1e-15.
   */
  def exp_continued_fraction(x : Rational, precision : Rational = RATIONAL_PRECISION) : Rational = {
    val xsqr = x*x
    def b(i : Int) = {
      if(i==0) Rational.one 
      else if(i==1) Rational(2,1) - x 
      else Rational(i*4-2)
    }
    def a(i : Int) = if(i==1) x*2 else xsqr
    new Algorithms.InfiniteGeneralisedContinuedFraction[Rational](a,b,precision).value
  }

  /** 
   * The lower incomplete gamma function computed by continued fraction
   * 
   * z^s e^-z /(s - sz/(s+1 + z/(s + 2 - (s+1)z/(s + 3 + 2z/... ))))
   * 
   * @param precision terms will be computed until a the result is modified by less than precision decimal places.  Result should be approximately this many decimal places accurate. Default 1e-15.
   */
  def lower_incomplete_gamma(s : Rational, z : Rational, precision : Rational = RATIONAL_PRECISION) : Rational = {
    def b(i : Int) = if(i<=0) Rational.zero else s + Rational(i-1)
    def a(i : Int) = if(i%2==1) Rational(i-1)*z else -(s + Rational(i-2))*z
    //this still needs to by multiplied by z^s e^z
    new Algorithms.InfiniteGeneralisedContinuedFraction[Rational](a,b,precision).value
  }
  
  /** 
   *  Natural logarithm.  Compute by continued fraction
   *  
   *  
   *  
   *  Converges quickly if z is not too close to 0.
   *  
   *  @param precision terms will be computed until a the result is modified by less than precision decimal places.  Result should be approximately this many decimal places accurate.  Default 1e-15.
   */ 
  def ln(z : Rational, precision : Rational = RATIONAL_PRECISION) : Rational = {
    def b(n: Int) = Rational(n,1)
    def a(n : Int) = if(n==1) z-1 else Rational((n/2)*(n/2),1)*(z-1) //floor being computed by integer division
    new Algorithms.InfiniteGeneralisedContinuedFraction[Rational](a,b,precision).value
  }
  
  
}
