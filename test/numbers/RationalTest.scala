/*
 * @author Robby McKilliam
 */

package numbers

import org.junit.Test
import org.junit.Assert._

class RationalTest {
  
  @Test
  def catchDemominatorZeroTest() {
    try{ val r = Rational(1,0) } catch {
      case e : RuntimeException => println("Denominator equal zero caught correctly")
      case _ : Throwable => fail("exception not correctly caught")
    }
  }
  
  @Test
  def compareTest() {
    assertTrue(Rational(1,2)==Rational(1,2))
    assertTrue(Rational(1,2)==Rational(2,4))
    assertTrue(Rational(-2,5)==Rational(2,-5))
    assertFalse(Rational(-2,5)==Rational(2,5))
    assertFalse(Rational(100,5)==Rational(2,70))
  }
  
  @Test
  def sumAndSubtractTest() {
    assertTrue((Rational(1,2) + Rational(3,4)) == Rational(5,4))
    assertTrue((Rational(1,2) - Rational(3,4)) == Rational(-1,4))
    assertTrue((Rational(1,2) + Integer(2)) == Rational(5,2))
    assertTrue((Rational(3,2) - Integer(2)) == -Rational(1,2))
  }
  
  @Test
  def mulitplyAndDivideTest() {
    assertTrue((Rational(1,2) * Rational(3,4)) == Rational(3,8))
    assertTrue((Rational(1,2) / Rational(4,3)) == Rational(3,8))
    assertTrue((Rational(1,3) * Integer(2)) == Rational(2,3))
    assertTrue((Rational(1,3) / Integer(2)) == Rational(1,6))
  }
  
    @Test def testImplicits() {
    val tol = 1e-7
    assertTrue( 1 + Rational.zero == Rational.one )
    assertTrue( Rational.zero + 1 == Rational.one )
    assertTrue( Rational.one*2 - 2 == Rational.zero )
    assertTrue( Rational.one*2 - 2 == Rational.zero)
    assertTrue( 2 - 2*Rational.one == Rational.zero)
    assertTrue( 2 - 2*Rational.one == Rational.zero)
    assertTrue( 2 - 3*Rational.one == -Rational.one)
    assertTrue( 2 - 3*Rational(3,4) == Rational(8-9,4))
  }

}
