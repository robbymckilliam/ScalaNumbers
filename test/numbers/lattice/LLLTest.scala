package numbers.lattice

import numbers.Rational
import numbers.RationalMatrix
import numbers.finite.Real
import numbers.finite.RealMatrix
import org.junit._
import Assert._

class LLLTest {

//  @Test
//  def RationalIdentityIsLLLReduced = {
//    val n = 3
//    val M = RationalMatrix.identity(n)
//    val (r, u) = LLL(M)
//    assertTrue( LLL.isReduced(r, Rational(3,4)) )
//    assertTrue( r == M )
//  }
//  
//  @Test
//  def RealIdentityIsLLLReduced = {
//    val n = 3
//    val M = RealMatrix.identity(n)
//    val (r, u) = LLL(M)
//    assertTrue( LLL.isReduced(r, Real(3.0/4)) )
//    assertTrue( r == M )
//  }
//  
  @Test
  def testIsHermiteReduced = {
    { //test with a largish Hilbert matrix
      val N = 4
      def f(m : Int, n : Int) = Rational(1,n+m+1)
      val A = RationalMatrix(f,N,N)
      val (r, m) = LLL(A)
      println(r)
      println(A*m)
      val (b,u) = r.orthogonalise
      println(b)
      println(u)
      assertTrue( Hermite.isReduced(r) ) //assert r is Hermite reduced
      //assertTrue( LLL.isReduced(r, Rational(3,4)) ) //assert r is Hermite reduced
      assertTrue( m.det == Rational.one )
      for( i <- m.indices ) assertTrue( m(i).isInteger )
      assertTrue( A*m == r )
    }
  }
  
//  @Test
//  def testWithRational = {
//    { //test with a largish Hilbert matrix
//      val N = 4
//      def f(m : Int, n : Int) = Rational(1,n+m+1)
//      val A = RationalMatrix(f,N,N)
//      val (r, m) = LLL(A)
//      println(r)
//      println(A*m)
//      val (b,u) = r.orthogonalise
//      println(b)
//      println(u)
//      assertTrue( Hermite.isReduced(r) ) //assert r is Hermite reduced
//      assertTrue( LLL.isReduced(r, Rational(3,4)) ) //assert r is Hermite reduced
//      assertTrue( m.det == Rational.one )
//      for( i <- m.indices ) assertTrue( m(i).isInteger )
//      assertTrue( A*m == r )
//    }
//  }
//  
//  @Test
//  def testWithRandomRational() = {
//    def f(m : Int, n : Int) = Rational(scala.util.Random.nextInt(5), scala.util.Random.nextInt(5)+1)
//    val N = 4
//    val A = RationalMatrix(f,N,N).backwitharray
//    val (r, m) = LLL(A)
//    assertTrue( Hermite.isReduced(r) ) //assert r is Hermitee reduced
//    assertTrue( LLL.isReduced(r, Rational(3,4)) ) //assert r is Hermitee reduced
//    assertTrue( m.det == Rational.one )
//    for( i <- m.indices ) assertTrue( m(i).isInteger )
//    assertTrue( A*m == r )
//  }
  
//  @Test
//  def detectsNotBasis = {
//    val n = 3
//    val b = Array(
//      Array(1,1,2),
//      Array(1,2,3),
//      Array(1,3,4)
//    )
//    val M = RationalMatrix( (m,n) => Rational(b(m)(n)), n, n )
//    try { 
//      val (r,u) = LLL(M)
//      println(r)
//      println(u)
//      println(u.det)
//      println(M*u)
//      assertTrue( LLL.isReduced(r, Rational(3,4)) )
//    } catch {
//      case e : RuntimeException => println("Not full rank exception caught correctly")
//      case _ : Throwable => fail("exception not correctly caught")
//    }
//  }

}
