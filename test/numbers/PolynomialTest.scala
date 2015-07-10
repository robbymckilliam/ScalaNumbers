/*
 * @author Robby McKilliam
 */

package numbers

import org.junit.Test
import org.junit.Assert._

class PolynomialTest {
  
  @Test
  def orderTest() {
    assertTrue(PolynomialRing(Array(Integer(2))).zero.order == -1)
    assertTrue(PolynomialRing(Array(Integer(2))).order == 0)
    assertTrue(PolynomialRing(Array(Integer(2), Integer(0))).order == 0)
    assertTrue(PolynomialRing(Array(Integer(2), Integer(1))).order == 1)
    assertTrue(PolynomialRing(Array(Integer(0), Integer(0))).order == -1)
    assertTrue(PolynomialRing(Array(Integer(0))).order == -1)
    assertTrue(PolynomialRing(Array(Integer(0), Integer(1))).order == 1)
    assertTrue(PolynomialRing(Array(Integer(0), Integer(0), Integer(1))).order == 2)
  }
  
  @Test
  def equalityTest() {
    {
      val a = PolynomialRing(Array(Integer(2)))
      val b = PolynomialRing(Array(Integer(3), Integer(2)))
      assertTrue( a != b )
    }
    {
      val a = PolynomialRing(Array(Integer(3), Integer(2)))
      val b = PolynomialRing(Array(Integer(3), Integer(2)))
      assertTrue( a == b )
    }
    {
      val a = PolynomialRing(Array(Integer(3), Integer(1)))
      val b = PolynomialRing(Array(Integer(3), Integer(2)))
      assertTrue( a != b )
    }
    {
      val a = PolynomialRing(Array(Integer(0)))
      val b = PolynomialRing(Array(Integer(0), Integer(0)))
      assertTrue( a == b )
    }
  }
  
  @Test
  def addTest() {
    {
      val a = PolynomialRing(Array(Integer(2)))
      val b = PolynomialRing(Array(Integer(3), Integer(2)))
      val c = PolynomialRing(Array(Integer(5), Integer(2)))
      assertTrue( a + b == c )
    }
    {
      val a = PolynomialRing(Array(Integer(2),-Integer(2)))
      val b = PolynomialRing(Array(Integer(3), Integer(2)))
      val c = PolynomialRing(Array(Integer(5)))
      assertTrue( a + b == c )
    }
    {
      val a = PolynomialRing(Array(Integer(2),-Integer(2)))
      val b = PolynomialRing(Array(Integer(3), Integer(2)))
      val c = PolynomialRing(Array(-Integer(1),-Integer(4)))
      assertTrue( a - b == c )
    }
  }

}
