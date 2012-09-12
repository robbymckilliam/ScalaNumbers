/*
 * @author Robby McKilliam
 */

package numbers

import org.junit.Test
import org.junit.Assert._
import numbers.EuclideanDomain.gcd
import numbers.EuclideanDomain.extended_gcd

class EuclideanDomainTest {
  
  @Test
  def IntgcdTest() {
    assertTrue(gcd(2,2)==2)
    assertTrue(gcd(7,311)==1)
    assertTrue(gcd(311,7)==1)
    assertTrue(gcd(10,255)==5)
    assertTrue(gcd(255,10)==5)
    assertTrue(gcd(2,-2)==2)
    assertTrue(gcd(49,7)==7)
  }

  @Test
  def IntextendedgcdTest() {
    val iters = 50
    val M = 1000
    for(i <- 1 to iters){
      val a = (new scala.util.Random).nextInt(M)
      val b = (new scala.util.Random).nextInt(M)
      val (n,m) = extended_gcd(a,b)
      //println(n,m,a,b)
      val d = gcd(a,b)
      assertEquals(a*n + b*m, d)
    }    
  }
  
}
