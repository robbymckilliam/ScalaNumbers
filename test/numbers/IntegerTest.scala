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
    assertTrue((Integer(-5) mod Integer(4)) == Integer(3))
  }
  
}
