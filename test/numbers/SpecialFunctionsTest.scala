/*
 * Testing of special functions.
 * @author Robby McKilliam
 */

package numbers

import org.junit._
import Assert._
import SpecialFunctions._
import RingWithUnity.pow

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
  
  @Test
  def expSeriesTest() {
    assertTrue( exp_series(Rational.zero) == Rational.one )
    //testing against Msathematicas Rationalize function
    assertTrue( (exp_series(Rational.one) - Rational(848456353L,312129649L)).norm < RATIONAL_PRECISION )
    assertTrue( (exp_series(-Rational.one) - Rational(140478290L,381859583L)).norm < RATIONAL_PRECISION )
    assertTrue( (exp_series(Rational(1,2)) - Rational(54516085L,33065677L)).norm < RATIONAL_PRECISION )
    assertFalse( (exp_series(Rational(0)) - Rational(54516085L,33065677L)).norm < RATIONAL_PRECISION )
  }
  
  @Test
  def expContinuedFractionTest() {
    assertTrue( exp_continued_fraction(Rational.zero) == Rational.one )
    //testing against Msathematica's Rationalize function
    assertTrue( (exp_continued_fraction(Rational.one) - Rational(848456353L,312129649L)).norm < RATIONAL_PRECISION )
    assertTrue( (exp_continued_fraction(-Rational.one) - Rational(140478290L,381859583L)).norm < RATIONAL_PRECISION )
    assertTrue( (exp_continued_fraction(Rational(1,2)) - Rational(54516085L,33065677L)).norm < RATIONAL_PRECISION )
    assertTrue( (exp_continued_fraction(Rational(10,1)) - Rational(9901850142761L,449543301L)).norm < RATIONAL_PRECISION )
    assertFalse( (exp_continued_fraction(Rational(0)) - Rational(9901850142761L,449543301L)).norm < RATIONAL_PRECISION )
    //assertTrue( (exp_continued_fraction(Rational(30,1)) - Rational("1277590680860887700008/449543301")).norm < RATIONAL_PRECISION )
  }

}
