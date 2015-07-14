package numbers.matrix

import numbers.Rational
import numbers.RationalMatrix
import numbers.finite.Complex
import numbers.finite.ComplexMatrix
import numbers.finite.Real
import numbers.finite.RectComplex
import org.junit._
import Assert._
import scala.collection.mutable.ArraySeq

class GramSchmidtTest {

  @Test
  def testProject = {
    { //test with finite precision Real
      val tol = Real(1e-7)
      val x = Array[Real](1.0,2.0,3.0)
      val p = Array[Real](1.0,1.0,1.0)
      val y = new ArraySeq[Real](3)
      val u = GramSchmidt.project(x,p,y)
      val ey = Array[Real](-1.0,0.0,1.0)
      val eu = 2.0
      assertTrue( (eu - u).norm < tol )
      for( i <- y.indices ) assertTrue( (y(i) - ey(i)).norm < tol )
      assertTrue( GramSchmidt.dot(p,y).norm < tol )
    }
    { //test with infinite precision Rational
      val x = Array[Rational](1,2,3)
      val p = Array[Rational](1,1,1)
      val y = new ArraySeq[Rational](3)
      val u = GramSchmidt.project(x,p,y)
      val ey = Array[Rational](-1,0,1)
      val eu = Rational(2)
      assertTrue( eu == u )
      for( i <- y.indices ) assertTrue( y(i) == ey(i) )
      assertTrue( GramSchmidt.dot(p,y) == Rational.zero )
    }
    { //test with finite precision Complex
      val tol = Real(1e-7)
      val x = Array[Complex]( RectComplex(1,1), RectComplex(2,0), RectComplex(3,0))
      val p = Array[Complex](Complex.one,Complex.one,Complex.one)
      val y = new ArraySeq[Complex](3)
      val u = GramSchmidt.project(x,p,y)
//      val ey = Array[Complex](-1,0,1)
      val eu = RectComplex(2,1.0/3)
//     println(u)
//     println(y)
      assertTrue( (eu - u).norm < tol )
//      for( i <- y.indices ) assertTrue( y(i) == ey(i) )
      assertTrue( GramSchmidt.dot(p,y).norm < tol )
    }
  }
  
  @Test
  def testProjectsZeroToSelf() {
    val x = Array[Rational](1,2,3)
    val p = Array[Rational](0,0,0)
    val y = new ArraySeq[Rational](3)
    val u = GramSchmidt.project(x,p,y)
    val ey = x
    val eu = Rational.zero
    assertTrue( eu == u )
    for( i <- y.indices ) assertTrue( y(i) == ey(i) )
    assertTrue( GramSchmidt.dot(p,y) == Rational.zero )
  }

  
  @Test
  def testGramSchmidt = {
    {
      val N = 3
      def f(m : Int, n : Int) = Rational(n+m+1)
      val A = RationalMatrix(f,N,N)
      val gs = new GramSchmidt(A)
      val B = gs.B
      val U = gs.U
      val D = B.transpose*B
      for(m <- 0 until N) for(n <- 0 until m) assertTrue(D(m,n) == D(0,0).zero) //assert columns of B are orthogonal
      assertTrue(A == B*U)
    }
    { //test with a largish Hilbert matrix
      val N = 7
      def f(m : Int, n : Int) = Rational(1,n+m+1)
      val A = RationalMatrix(f,N,N)
      val gs = new GramSchmidt(A)
      val B = gs.B
      val U = gs.U
      val D = B.transpose*B
      for(m <- 0 until N) for(n <- 0 until m) assertTrue(D(m,n) == D(0,0).zero) //assert columns of B are orthogonal
      assertTrue(A == B*U)
    }
    { //test with Complex
      val tol = Real(1e-7)
      val N = 3
      def f(m : Int, n : Int) = RectComplex(n-m+2,n+m+1)
      val A = ComplexMatrix(f,N,N)
      val gs = new GramSchmidt(A)
      val B = gs.B
      val U = gs.U
      val D = B.transpose*B
      for(m <- 0 until N) for(n <- 0 until m) assertTrue( (D(m,n) - D(0,0).zero).norm < tol) //assert columns of B are orthogonal
      val diff = A - B*U
      for(i <- diff.indices) assertTrue( diff(i).norm < tol )
    }
    { //test with orthogonalise method
      val N = 7
      def f(m : Int, n : Int) = Rational(1,n+m+1)
      val A = RationalMatrix(f,N,N)
      val (b, u) = A.orthogonalise
      val D = b.transpose*b
      for(m <- 0 until N) for(n <- 0 until m) assertTrue(D(m,n) == D(0,0).zero) //assert columns of B are orthogonal
      assertTrue(A == b*u)
    }
  }
  
  @Test
  def GramSchmidtMoreRowsThanColumnsTest() {
    def f(m : Int, n : Int) = Rational(scala.util.Random.nextInt(5), scala.util.Random.nextInt(5)+1)
    val M = 4
    val N = 3
    val A = RationalMatrix(f,M,N).backwitharray
    val (b, u) = A.orthogonalise
    val D = b.transpose*b
    for(m <- 0 until N) for(n <- 0 until m) assertTrue(D(m,n) == D(0,0).zero) //assert columns of B are orthogonal
    assertTrue(A == b*u)
  }
  
  @Test
  def GramSchmidtMoreColumnsThanRowsTest() {
    def f(m : Int, n : Int) = Rational(scala.util.Random.nextInt(5), scala.util.Random.nextInt(5)+1)
    val M = 3
    val N = 4
    val A = RationalMatrix(f,M,N).backwitharray
    val (b, u) = A.orthogonalise
    val D = b.transpose*b
    for(m <- 0 until N) for(n <- 0 until m) assertTrue(D(m,n) == D(0,0).zero) //assert columns of B are orthogonal
    assertTrue(A == b*u)
  }
  
  
  @Test
  def rationalRandomGramSchmidtTest() {
    def f(m : Int, n : Int) = Rational(scala.util.Random.nextInt(100), scala.util.Random.nextInt(100)+1)
    val sizes = List( (5,5), (5,4) )
    for( (m_, n_) <- sizes ){
      val A = RationalMatrix(f,m_,n_).backwitharray
      val (b, u) = A.orthogonalise
      val D = b.transpose*b
      for(m <- 0 until n_) for(n <- 0 until m) assertTrue(D(m,n) == D(0,0).zero) //assert columns of B are orthogonal
      assertTrue(A == b*u)
    }
  }
  
}
