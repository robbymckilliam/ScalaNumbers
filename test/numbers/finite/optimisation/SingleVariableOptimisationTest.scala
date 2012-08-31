package numbers.finite.optimisation
package numbers.finite.optimisation.SingleVariableOptimisation

import org.junit.Test;
import org.junit.Assert._;

class SingleVariableOptimisationTest {

  val tol = 1e-7
	
  @Test
  def QuadraticTest() = {
    val f : Double => Double = x => x*x
    val (fx, x) = new Brent(f, -10.0, 8.0, 5.0, tol).min
    println(fx, x)
    assertEquals(0.0, fx, tol)
    assertEquals(0.0, x, tol)
    
    val (fxm, xm) = SingleVariableOptimisation.fmin(f, -11.0, 5.0, tol)
    assertEquals(0.0, fxm, tol)
    assertEquals(0.0, xm, tol)
    
  }
  
  @Test
  def QuarticTest() = {
    val f : Double => Double = x => x*x*x*x
    val (fx, x) = new Brent(f, -2, 8.0, 10.0, tol).min
    println(fx, x)
    assertEquals(0.0, fx, tol)
    assertEquals(0.0, x, tol)
    
    val (fxm, xm) = SingleVariableOptimisation.fmin(f, -11.0, 5.0, tol)
    assertEquals(0.0, fxm, tol)
    assertEquals(0.0, xm, tol)
  }
  
}