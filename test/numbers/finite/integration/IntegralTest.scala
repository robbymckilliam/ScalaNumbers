package numbers.finite.integration

import org.junit.Test
import org.junit.Assert._
//import numbers.finite.Complex

class RealIntegralTest {

  val tolerance = 1e-2
      
  @Test
  def TrapezoidalTest() {
    def f(x : Double) = x*x
    def fint(a : Double, b: Double) = b*b*b/3 - a*a*a/3
    val a = -4.0 
    val b = 3.0
    val N = 500
    val aint = numbers.finite.integration.RealIntegral.trapezoidal(f(_),a,b,N)
    assertEquals(fint(a,b),aint,tolerance)
  }
  
//  @Test
//  def ComplexTrapezoidalTest() {
//    //test with rectangular function
//    def f(x : Double) : Complex = if(x.abs > 1) Complex.zero else Complex.one
//    def fint = Complex.one * 2
//    val a = -4.0 
//    val b = 3.0
//    val N = 500
//    val aint = numbers.finite.integration.ComplexIntegral.trapezoidal(f(_) ,a, b, N)
//    assertEquals(fint.real,aint.real,tolerance)
//    assertEquals(fint.imag,aint.imag,tolerance)
//  }
  
}