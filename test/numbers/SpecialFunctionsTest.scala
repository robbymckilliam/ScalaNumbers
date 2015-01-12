/*
 * Testing of special functions.
 * @author Robby McKilliam
 */

package numbers

import org.junit._
import Assert._
import SpecialFunctions._

class SpecialFunctionsTest {

  val TOL = pow(Rational(1,10),14)
  
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
    assertTrue( (exp_series(Rational.one) - Rational(848456353L,312129649L)).norm < TOL )
    assertTrue( (exp_series(-Rational.one) - Rational(140478290L,381859583L)).norm < TOL)
    assertTrue( (exp_series(Rational(1,2)) - Rational(54516085L,33065677L)).norm < TOL )
    assertFalse( (exp_series(Rational(0)) - Rational(54516085L,33065677L)).norm < TOL )
  }
  
  @Test
  def expContinuedFractionTest() {
    assertTrue( exp_continued_fraction(Rational.zero) == Rational.one )
    //testing against Msathematica's Rationalize function
    assertTrue( (exp_continued_fraction(Rational.one) - Rational(848456353L,312129649L)).norm < TOL )
    assertTrue( (exp_continued_fraction(-Rational.one) - Rational(140478290L,381859583L)).norm < TOL )
    assertTrue( (exp_continued_fraction(Rational(1,2)) - Rational(54516085L,33065677L)).norm < TOL )
    assertTrue( (exp_continued_fraction(Rational(10,1)) - Rational(9901850142761L,449543301L)).norm < TOL )
    assertFalse( (exp_continued_fraction(Rational(0)) - Rational(9901850142761L,449543301L)).norm < TOL )
    println(exp_continued_fraction(Rational(30,1)))
    println((exp_continued_fraction(Rational(30,1))- Rational("1277590680860887700008/449543301")).toDouble)
    //assertTrue( (exp_continued_fraction(Rational(30,1)) - Rational("1277590680860887700008/449543301")).norm < RATIONAL_PRECISION )
  }
  
  @Test
  def lnTest() {
    assertTrue( ln(Rational.one) == Rational.zero )
    assertTrue( (ln(Rational(1,2)) - -Rational("49180508/70952475")).norm < TOL )
    assertTrue( (ln(Rational(2)) - Rational("6847196937/9878417065")).norm < TOL )
  }
  
  @Test
  def powIntegerTest() {
    assertTrue( pow(Rational.one,10) == Rational.one)
    assertTrue( pow(Rational.zero,10) == Rational.zero)
    assertTrue( pow(Rational(1,2),3) == Rational(1,8))
    assertTrue( pow(-Rational(1,2),3) == -Rational(1,8))
    assertTrue( pow(Rational(1,2),-3) == Rational(8,1))
  }

//  @Test
//  def powRationalTest() {
//    assertTrue( pow(Rational.one,Rational(3,5)) == Rational.one)
//    assertTrue( pow(Rational.zero,Rational(5,3)) == Rational.zero)
//    assertTrue( pow(Rational(4,1),Rational(1,2)) == Rational(2))
//    assertTrue( pow(Rational(8),Rational(1,3)) == -Rational(2))
//  }
//  
//  @Test
//  def lower_incomplete_Gamma_Test() {
//    assertTrue( pow(Rational.one,Rational(3,5)) == Rational.one)
//  }
  
}
