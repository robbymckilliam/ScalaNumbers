package numbers.integration

/** 
 * One dimensional integral of
 * a signal variable function mapping the real
 * numbers to the real numbers.
 */
object RealIntegral {

  /** 
   * Approximates the integral of f from a to b using the trapezoidal 
   * rule with N intervals
   * */
  def trapezoidal(f : Double => Double, a : Double, b : Double, N : Int) : Double = {
    val del = (b - a)/N
    val inner = (1 to N-1).map(n => f(a + n*del)).foldLeft(0.0)((s,v) => s+2*v)
    return del/2 * ( inner + f(a) + f(b) )
  }
  
}