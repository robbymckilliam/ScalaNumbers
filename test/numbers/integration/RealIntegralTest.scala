package numbers.integration

import org.junit.Test
import org.junit.Assert._
import numbers.integration.RealIntegral._

class RealIntegralTest {

    val tolerance = 1e-2
    
    def f(x : Double) = x*x
    def fint(a : Double, b: Double) = b*b*b/3 - a*a*a/3
    
    @Test
    def TrapezoidalTest() {
      val a = -4.0 
      val b = 3.0
      val N = 500
	  val aint = trapezoidal(f,a,b,N)
	  assertEquals(fint(a,b),aint,tolerance)
    }
  
}