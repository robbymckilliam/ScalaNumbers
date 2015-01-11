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
   * @param precision terms will be computed until less than precision decima places.  Result should be approximately this many decimal places accurate.
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
   * Converges even when x is large.
   * 
   * @param x   The exponent. Will compute e^x
   * @param precision terms will be computed until less than precision decima places.  Result should be approximately this many decimal places accurate.
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
//  
//  def exp(x : Rational, precision : Rational = RATIONAL_PRECISION) : Rational = {
//    
//  }
  
  
}
