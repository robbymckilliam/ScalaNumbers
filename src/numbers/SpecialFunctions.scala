/*
 * Special functions, exp, gamma, factorial, etc.
 * @author Robby McKilliam 11/1/2015
 */

package numbers

import scala.annotation.tailrec

object SpecialFunctions {

  /// Rational precision defaults to 15 decimal
  def RATIONAL_PRECISION = Rational(1,1000000000000000L)
  
  ///Factorial of integer n
  def factorial(n : Integer) : Integer = {
    if(n<Integer.zero) throw new RuntimeException("Cannot compute factorial of negative number.")
    @tailrec def f(i : Integer, v : Integer) : Integer = if(i==Integer.zero) return v else return f(i-1,v*i)
    return f(n,Integer.one)
  }  
  
//  def exp_series(x : Rational, precision : Rational = RATIONAL_PRECISION) = {
//    @tailrec def f()
//  }
//  
//  def exp(x : Rational, precision : Rational = RATIONAL_PRECISION) : Rational = {
//    
//  }
  
  
}
