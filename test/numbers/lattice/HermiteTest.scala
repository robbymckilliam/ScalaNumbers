package numbers.lattice

import numbers.Rational
import numbers.RationalMatrix
import numbers.finite.Real
import numbers.finite.RealMatrix
import org.junit._
import Assert._

class HermiteTest {

  @Test
  def testWithRational = {
    { //test with a largish Hilbert matrix
      val N = 7
      def f(m : Int, n : Int) = Rational(1,n+m+1)
      val A = RationalMatrix(f,N,N)
      val (r, m) = Hermite(A)
      assertTrue( Hermite.isReduced(r) ) //assert r is Hermitee reduced
      assertTrue( m.det == Rational.one )
      for( i <- m.indices ) assertTrue( m(i).isInteger )
      assertTrue( A*m == r )
    }
  }
  
  @Test
  def testWithReal = {
    { //test with a smallHilbert matrix
      val tol = 1e-7
      val N = 4
      def f(m : Int, n : Int) = Real(1.0/(n+m+1))
      val A = RealMatrix(f,N,N)
      val (r, m) = Hermite(A)
      assertTrue( Hermite.isReduced(r) ) //assert r is Hermitee reduced
      assertFalse( (m.det - Real.one).normlarger(tol) )
      for( i <- m.indices ) assertTrue( m(i).isInteger(tol) )
      val D = A*m - r
      for(m <- 0 until N) for(n <- 0 until N) assertFalse(D(m,n).normlarger(tol)) //assert columns of B are orthogonal
    }
  }

  @Test
  def HermiteMoreRowsThanColumnsTest() {
    def f(m : Int, n : Int) = Rational(scala.util.Random.nextInt(5), scala.util.Random.nextInt(5)+1)
    val M = 4
    val N = 3
    val A = RationalMatrix(f,M,N).backwitharray
    val (r, m) = Hermite(A)
    assertTrue( Hermite.isReduced(r) ) //assert r is Hermitee reduced
    assertTrue( m.det == Rational.one )
    for( i <- m.indices ) assertTrue( m(i).isInteger )
    assertTrue( A*m == r )
  }
  
  
}
