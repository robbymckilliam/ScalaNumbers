/*
 * @author Robby McKilliam
 */

package numbers

import org.junit.Test
import org.junit.Assert._

class IntegerTest {
  
  @Test
  def compareTest() {
    assertTrue(Integer(2)==Integer(2))
    assertFalse(Integer(3)==Integer(10))
    assertTrue(Integer(-5)==Integer(-5))
    assertFalse(Integer(-5)==Integer(2))
    assertTrue(Integer(-5)<Integer(2))
    assertTrue(Integer(-5)<=Integer(2))
    assertFalse(Integer(-5)>Integer(2))
    assertTrue(Integer(10)<=Integer(10))
    assertTrue(Integer(10)>=Integer(10))
    assertTrue(Integer(10)!=Integer(20))
    assertTrue(!(Integer(10)!=Integer(10)))
  }

  @Test
  def addTest() {
    assertTrue((Integer(1) + Integer(2)) == Integer(3))
  }
  
  @Test
  def modTest() {
    assertTrue((Integer(1) mod Integer(4)) == Integer(1))
    assertTrue((Integer(5) mod Integer(4)) == Integer(1))
    assertTrue((Integer(-5) mod Integer(4)) == Integer(3))
    assertTrue((Integer(-5) mod Integer(-4)) == -Integer(1))
    assertTrue((Integer(-5) mod Integer(-10)) == -Integer(5))
  }
  
  @Test
  def normlargerTest() {
    assertFalse(Integer(1) normlarger Integer(4))
    assertTrue(Integer(4) normlarger Integer(1))
    assertTrue(Integer(-5) normlarger Integer(4))
    assertFalse(Integer(-5) normlarger Integer(-5))
  }
  
  @Test def IntImplicits() {
    val tol = 1e-7
    assertTrue( 1 + Integer.zero == Integer.one )
    assertTrue( Integer.zero + 1 == Integer.one )
    assertTrue( Integer.one*2 - 2 == Integer.zero )
    assertTrue( Integer.one*2 - 2 == Integer.zero)
    assertTrue( 2 - 2*Integer.one == Integer.zero)
    assertTrue( 2 - 2*Integer.one == Integer.zero)
    assertTrue( 2 - 3*Integer.one == -Integer.one)
  }
  
  @Test
  def testIntegerToDouble = {
    val tol = 1e-8
    assertTrue( (Integer(10).toDouble - 10.0).abs < tol )
    assertTrue( (Integer(-10).toDouble + 10.0).abs < tol )
    assertTrue( (Integer(2).toDouble - 2.0).abs < tol )
  }
  
  @Test
  def testIntegerPow = {
    assertTrue( RingWithUnity.pow(Integer(10),2) == Integer(100) )
    assertTrue( RingWithUnity.pow(Integer(3),7) == Integer(2187) )
  }
  
}
