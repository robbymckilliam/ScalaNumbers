package numbers.finite.optimisation

import numbers.finite.optimisation.SingleVariableOptimisation.fmin
import numbers.finite.optimisation.SingleVariableOptimisation.fmax

import org.junit.Test;
import org.junit.Assert._;

class SingleVariableOptimisationTest {

  val tol = 1e-7
	
  @Test
  def QuadraticTest() = {
    val f : Double => Double = x => (x-2.0)*(x-2.0)
    val (fxm, xm) = fmin(f, -4.0,1.0,5.0, tol)
    assertEquals(0.0, fxm, tol)
    assertEquals(2.0, xm, tol)
  }
  
  @Test
  def QuarticTest() = {
    val f : Double => Double = x => x*x*x*x
    val (fxm, xm) = fmin(f, -2.0, 1.0, 2.0, tol)
    assertEquals(0.0, fxm, tol)
    assertEquals(0.0, xm, tol)
    
    val (fxmd, xmd) = fmin(f, -10.0,3.0, 9.0)
    assertEquals(0.0, fxmd, 1e-6)
    assertEquals(0.0, xmd, 1e-6)
    
  }
  
  @Test
  def fmaxTest() = {
    val f : Double => Double = x => -(x-2)*(x-2)
    val (fxm, xm) = fmax(f, -11.0,4.0,8.0,tol)
    assertEquals(0.0, fxm, tol)
    assertEquals(2.0, xm, tol)
    
    val (fxmd, xmd) = fmax(f, -11.0,4.0,13.0)
    assertEquals(0.0, fxmd, 1e-6)
    assertEquals(2.0, xmd, 1e-6)
    
  }
  
}