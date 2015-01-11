/*
 * Testing of special functions.
 * @author Robby McKilliam
 */

package numbers

import org.junit._
import Assert._
import SpecialFunctions._

class SpecialFunctionsTest {

  @Test
  def catchFactorialNegativeTest() {
    try{ val r = factorial(-1) } catch {
      case e : RuntimeException => println("factorial of negative caught correctly")
      case _ : Throwable => fail("exception not correctly caught")
    }
  }
  
  @Test
  def factorialTest() {
    assertTrue(factorial(0)==Integer.one)
    assertTrue(factorial(1)==Integer.one)
    assertTrue(factorial(2)==Integer(2))
    assertTrue(factorial(3)==Integer(6))
    assertTrue(factorial(4)==Integer(24))
    assertTrue(factorial(10)==(factorial(9)*10))
  }

}
