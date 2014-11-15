package numbers.finite

import org.junit.Test;
import org.junit.Assert._;

class RealTest {
  
  @Test def operateWithDoubleTest() {
    val tol = 1e-7
    assertTrue(Real(1.0).norm==Real(1.0))
    assertTrue(Real(-1.0).norm==Real(1.0))
    assertTrue( (Real(1.0) * 20.0 - 20.0) < Real(tol) )
    assertTrue( (Real(1.0) * 20.0 - 20.0) < tol )
    assertTrue( (Real(1.0) * 20.0 - 20.0) < 1 )
    
  }
  
  @Test def doubleImplicits() {
    val tol = 1e-7
    assertTrue( 1.0 + Real.zero == Real.one )
    assertTrue( Real.zero + 1.0 == Real.one )
    assertTrue( (Real.one * 2.0 - Real(2.0)).norm < tol )
    assertTrue( (2.0*Real.one - Real(2.0)).norm < tol )
    assertTrue( (2.0 - 2.0*Real.one).norm < tol )
    assertTrue( ((2.0 - 3.0*Real.one) + 1.0).norm < tol )
  }
  
   @Test def IntImplicits() {
    val tol = 1e-7
    assertTrue( 1 + Real.zero == Real.one )
    assertTrue( Real.zero + 1 == Real(1) )
    assertTrue( (Real.one * 2 - Real(2)).norm < tol )
    assertTrue( (2*Real.one - Real(2)).norm < tol )
    assertTrue( (2 - 2*Real.one).norm < tol )
    assertTrue( (2 - 3*Real.one + 1).norm < tol )
    assertTrue( (2 - 3*Real.one + Real(1)).norm < tol )
  }
  
  @Test
  def finite_continued_fraction_test = {
    {
      val r = numbers.Rational(10,2)
      val a = r.continued_fraction
      val rd = 5.0
      val rc = Real.from_continued_fraction(a)
      assertTrue((rd-rc).norm < 1e-10)
    }
    { //test comes from Wikipedia
      val r = numbers.Rational(415,93)
      val a = r.continued_fraction
      val rd = 415.0/93.0
      val rc = Real.from_continued_fraction(a)
      assertTrue((rd-rc).norm < 1e-10)
    }
    { //test comes from Wikipedia
      val r = numbers.Rational(-412315,213)
      val a = r.continued_fraction
      val rd = -412315.0/213.0
      val rc = Real.from_continued_fraction(a)
      assertTrue((rd-rc).norm < 1e-10)
    }
  }

  @Test
  def infinite_continued_fraction_test = {
    val tols = (1 to 8).map( p => Real(scala.math.pow(10,p)) )
    for( tol <- tols ) { //test with golden ratio
      def a(i : Int) = 1
      val rc = Real.from_continued_fraction(a, tol)
      val phi = (scala.math.sqrt(5) + 1)/2.0 //double precision version of golden ratio
      assertTrue( (rc - phi).norm < tol )
    }
    for( tol <- tols ) { //test square root of 2
      def a(i : Int) = if(i==0) 1 else 2
      val rc = Real.from_continued_fraction(a, tol)
      assertTrue( (rc - scala.math.sqrt(2)).norm < tol )
    }
  }
  
}
