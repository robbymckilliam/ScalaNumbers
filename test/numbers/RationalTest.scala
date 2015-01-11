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
  def constructFromStringTest() {
    assertTrue(Rational("1","2")==Rational(1,2))
    assertTrue(Rational("1/2")==Rational(1,2))
    val r = Rational(12314,1431)
    assertTrue(Rational(r.toString)==r)
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
  
  @Test
  def testRationalToDouble = {
    val tol = 1e-10
    assertTrue( (Rational(10,2).toDouble - 5.0).abs < tol )
    assertTrue( (Rational(10,1).toDouble - 10.0).abs < tol )
    assertTrue( (Rational(-2,3).toDouble + 2.0/3.0).abs < tol )
//    {//now do a serious test of toDouble
      val p = new bignums.BigInteger("100000000000000000000000000000000000000000000001")
      val q = new bignums.BigInteger("100000000000000000000000000000000000000000000")
      val r = Rational(Integer(p), Integer(q))
      val rd = 1000.0
      println(r.toDouble, rd)
      assertTrue( (r.toDouble - rd).abs < tol )
//    }
  }
  
  @Test
  def testFloor = {
    assertTrue( Rational(10,2).floor == Integer(5) )
    assertTrue( Rational(3,2).floor == Integer.one )
    assertTrue( Rational(-3,2).floor == -Integer(2) )
  }
  
  @Test
  def testCeil = {
    assertTrue( Rational(10,2).ceil == Integer(5) )
    assertTrue( Rational(3,2).ceil == Integer(2) )
    assertTrue( Rational(5,2).ceil == Integer(3) )
    assertTrue( Rational(-3,2).ceil == -Integer(1) )
  }
  
  @Test
  def testRound = {
    assertTrue( Rational(10,2).round == Integer(5) )
    assertTrue( Rational(3,2).round == Integer(2) )
    assertTrue( Rational(5,3).round == Integer(2) )
    assertTrue( Rational(4,3).round == Integer.one )
    assertTrue( Rational(-3,4).round == -Integer.one )
    assertTrue( Rational(-5,4).round == -Integer.one )
    assertTrue( Rational(-1,2).round == Integer.zero )
  }
  
  @Test
  def finite_continued_fraction_test = {
    {
      val r = Rational(10,2)
      val a = r.continued_fraction
      assertTrue(a.size == 1)
      assertTrue(a(0) == Integer(5))
      val rc = Rational.from_continued_fraction(a)
      assertTrue(r == rc)
    }
    { //test comes from Wikipedia
      val r = Rational(415,93)
      val a = r.continued_fraction
      assertTrue(a.size == 4)
      assertTrue(a(0) == Integer(4))
      assertTrue(a(1) == Integer(2))
      assertTrue(a(2) == Integer(6))
      assertTrue(a(3) == Integer(7))
      val rc = Rational.from_continued_fraction(a)
      assertTrue(r == rc)
    }
    { //test comes from Wikipedia
      val r = Rational(-41532,76)
      val a = r.continued_fraction
      assertTrue(a.size > 0)
      val rc = Rational.from_continued_fraction(a)
      assertTrue(r == rc)
    }
  }
  
  @Test
  def infinite_continued_fraction_test = {
    val tols = (1 to 8).map( p => Rational(1,scala.math.pow(10,p).round))
    for( tol <- tols ) { //test with golden ratio
      def a(i : Int) = 1
      val rc = Rational.from_continued_fraction(a, tol)
      val phi = (scala.math.sqrt(5) + 1)/2.0 //double precision version of golden ratio
      assertTrue( (rc.toDouble - phi).abs < tol.toDouble )
    }
    for( tol <- tols ) { //test square root of 2
      def a(i : Int) = if(i==0) 1 else 2
      val rc = Rational.from_continued_fraction(a, tol)
      assertTrue( (rc.toDouble - scala.math.sqrt(2)).abs < tol.toDouble )
    }
  }
  
  @Test
  def testPow = {
    assertTrue( RingWithUnity.pow(Rational(10,5),2) == Rational(4,1) )
    assertTrue( RingWithUnity.pow(Rational(3,4),7) == Rational(2187,16384) )
  }
  
}
