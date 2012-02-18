package numbers.integration

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
    val inner = (1 to N-1).map(n => f(a + n*del)).foldLeft(0.0)((s,v) => s+2*v)
    return del/2 * ( inner + f(a) + f(b) )
  }
  
}

/** 
 * One dimensional integral of a single variable function mapping the real
 * numbers to the real numbers.
 */
trait RealIntegral {
  def integrate(a : Double, b : Double): Double
}

/** Trapezoidal integration of the function f with N steps */
class Trapezoid(f : Double => Double, N : Int) extends RealIntegral {
  def integrate(a: Double, b : Double) : Double = { RealIntegral.trapezoidal(f,a,b,N) }
}
