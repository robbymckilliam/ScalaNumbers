package numbers.finite.integration

import numbers.finite.Complex

/**
 * Static methods for real numerical integration, just for convenience
 */
object RealIntegral {

  /** 
   * Approximates the integral of f from a to b using the trapezoidal 
   * rule with N intervals
   * */
  def trapezoidal(f : Double => Double, a : Double, b : Double, N : Int) : Double = {
    val del = (b - a)/N
    val inner = (1 to N-1).foldLeft(0.0)((s,n) => s+2*f(a + n*del))
    return del/2 * ( inner + f(a) + f(b) )
  }
  
  /** 
   * Approximates the integral of f from a to b using the trapezoidal 
   * rule with N intervals.  For complex numbers.
   * */
  def trapezoidal(f : Double => Complex, a : Double, b : Double, N : Int) : Complex = {
    val del = (b - a)/N
    val inner = (1 to N-1).foldLeft(Complex.zero)((s,n) => s+f(a + n*del)*2)
    return ( inner + f(a) + f(b) ) * del/2
  }
  
}
