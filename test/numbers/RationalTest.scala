/*
 * @author Robby McKilliam
 */

package numbers

import org.junit.Test
import org.junit.Assert._
import numbers.Rational

class RationalTest {
  
  @Test
  def catchDemominatorZeroTest() {
    try{ val r = Rational(1,0) } catch {
      case e : RuntimeException => println(e.getMessage)
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
  def sumTest() {
    assertTrue((Rational(1,2) + Rational(3,4)) == Rational(5,4))
  }
  
  @Test
  def mulitplyAndDivideTest() {
    assertTrue((Rational(1,2) * Rational(3,4)) == Rational(3,8))
  }

}
