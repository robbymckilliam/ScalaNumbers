/*
 * @author Robby McKilliam
 */

package numbers

import org.junit.Test
import org.junit.Assert._
import numbers.EuclideanDomain.gcd
import numbers.EuclideanDomain.lcm
import numbers.EuclideanDomain.extended_gcd

class EuclideanDomainTest {
  
  @Test
  def IntgcdandlcmTest() {
    assertTrue(gcd(2,2)==2)
    assertTrue(gcd(7,311)==1)
    assertTrue(gcd(311,7)==1)
    assertTrue(gcd(10,255)==5)
    assertTrue(gcd(255,10)==5)
    assertTrue(gcd(2,-2)==2)
    assertTrue(gcd(49,7)==7)
    assertTrue(gcd(List(1,2,3,4))==1)
    assertTrue(gcd(List(3,12,15,21))==3)
    assertTrue(lcm(2,4)==4)
    assertTrue(lcm(3,4)==12)
    assertTrue(lcm(List(2,3,4,6))==12)
    assertTrue(lcm(List(3,4,5))==60)
  }

  
  @Test
  def LonggcdandlcmTest() {
    assertTrue(gcd(2.toLong,2.toLong)==2.toLong)
    assertTrue(gcd(7.toLong,311.toLong)==1.toLong)
    assertTrue(gcd(311.toLong,7.toLong)==1.toLong)
    assertTrue(gcd(10.toLong,255.toLong)==5.toLong)
    assertTrue(gcd(255.toLong,10.toLong)==5.toLong)
    assertTrue(gcd(2.toLong,-2.toLong)==2.toLong)
    assertTrue(gcd(49.toLong,7.toLong)==7.toLong)
    assertTrue(gcd(List(1L,2L,3L,4L))==1L)
    assertTrue(gcd(List(3L,12L,15L,21L))==3L)
    assertTrue(lcm(2L,4L)==4L)
    assertTrue(lcm(3L,4L)==12L)
    assertTrue(lcm(List(2L,3L,4L,6L))==12L)
    assertTrue(lcm(List(3L,4L,5L))==60L)
  }
  
  @Test
  def IntegergcdandlcmTest() {
    assertTrue(gcd(Integer(2),Integer(2))==Integer(2))
    assertTrue(gcd(Integer(7),Integer(311))==Integer(1))
    assertTrue(gcd(Integer(49),Integer(7))==Integer(7))
    assertTrue(gcd(List(Integer(1),Integer(2),Integer(3),Integer(4)))==Integer(1))
    assertTrue(gcd(List(Integer(3),Integer(12),Integer(15),Integer(21)))==Integer(3))
    assertTrue(lcm(Integer(2),Integer(4))==Integer(4))
    assertTrue(lcm(Integer(3),Integer(4))==Integer(12))
    assertTrue(lcm(List(Integer(2),Integer(3),Integer(4),Integer(6)))==Integer(12))
    assertTrue(lcm(List(Integer(3),Integer(4),Integer(5)))==Integer(60))
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
  
  @Test
  def LongextendedgcdTest() {
    val iters = 50
    val M = 100000
    for(i <- 1 to iters){
      val a = (new scala.util.Random).nextInt(M).toLong
      val b = (new scala.util.Random).nextInt(M).toLong
      val (n,m) = extended_gcd(a,b)
      //println(n,m,a,b)
      val d = gcd(a,b)
      assertEquals(a*n + b*m, d)
    }    
  }
  
  @Test
  def IntegerextendedgcdTest() {
    {
      val (s,t) = extended_gcd(Integer(3),Integer(6))
      assertTrue( (s*3 + t*6) == Integer(3) )
    }
    val iters = 50
    val M = 100000
    for(i <- 1 to iters){
      val a = Integer((new scala.util.Random).nextInt(M))
      val b = Integer((new scala.util.Random).nextInt(M))
      val (n,m) = extended_gcd(a,b)
      //println(n,m,a,b)
      val d = gcd(a,b)
      assertTrue((a*n + b*m) == d)
    }    
  }
  
  @Test
  def extendedgcdMoreThan2Test() {
    {
      val a = List(Integer(2),Integer(3),Integer(4),Integer(5),Integer(6))
      val b = extended_gcd(a)
      val g = gcd(a)
      val sumab = a.indices.foldLeft(Integer.zero)( (s, i) => s + a(i)*b(i) )
      assertTrue( sumab == g )
    }
    {
      val a = List(Integer(3),Integer(6),Integer(12))
      val b = extended_gcd(a)
      val g = gcd(a)
      val sumab = a.indices.foldLeft(Integer.zero)( (s, i) => s + a(i)*b(i) )
      assertTrue( sumab == g )
    }
    {
      val L = 6
      val iters = 100
      val M = 10000
      for(i <- 1 to iters){
        val a = (1 to L).map( l => Integer((new scala.util.Random).nextInt(M)) )
        val b = extended_gcd(a)
        val g = gcd(a)
        val sumab = a.indices.foldLeft(Integer.zero)( (s, i) => s + a(i)*b(i) )
        assertTrue( sumab == g )
      }    
    }
  }
  
}
