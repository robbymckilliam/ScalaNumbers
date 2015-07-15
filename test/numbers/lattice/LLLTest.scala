package numbers.lattice

import numbers.Rational
import numbers.RationalMatrix
import numbers.finite.Real
import numbers.finite.RealMatrix
import numbers.matrix.LU
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
/*  @Test
  def testIsHermiteReduced = {
    { //test with a largish Hilbert matrix
      val N = 5
      def f(m : Int, n : Int) = Rational(1,n+m+1)
      val A = RationalMatrix(f,N,N)
      val (r, m) = LLL(A)
      assertTrue( Hermite.isReduced(r) ) //assert r is Hermite reduced
      assertTrue( m.det.norm == Rational.one )
      for( i <- m.indices ) assertTrue( m(i).isInteger )
      assertTrue( A*m == r )
    }
  }
   
  @Test
  def testIsLovasReduced = {
    { //test with a largish Hilbert matrix
      val N = 6
      def f(m : Int, n : Int) = Rational(1,n+m+1)
      val A = RationalMatrix(f,N,N)
      val (r, m) = LLL(A)
      assertTrue( Hermite.isReduced(r) ) //assert r is Hermite reduced
      assertTrue( LLL.isReduced(r, Rational(3,4)) ) //assert r is LLL reduced
      assertTrue( m.det.norm == Rational.one )
      for( i <- m.indices ) assertTrue( m(i).isInteger )
      assertTrue( A*m == r )
    }
  }
 */ 
  @Test
  def testWithRandomRational() = {
    def f(m : Int, n : Int) = Rational(scala.util.Random.nextInt(50), scala.util.Random.nextInt(50)+1)
    for( N <- List( 4, 5, 7) ) {
    //val N = 5 
    val A = RationalMatrix(f,N,N).backwitharray
      if(!(new LU(A).isSingular)){ //only run test if this random matrix is not singular 
        val (r, m) = LLL(A)
        val (b,u) = r.orthogonalise
      //println(r)
      //println(m)
      //println(m.det)
   //   println(b.t*b)
   //   println(u)
   //   println( A*m )
   //   println
   //   println
        assertTrue( Hermite.isReduced(r) ) //assert r is Hermitee reduced
        assertTrue( LLL.isReduced(r, Rational(3,4)) ) //assert r is LLL reduced
        assertTrue( m.det.norm == Rational.one )
        for( i <- m.indices ) assertTrue( m(i).isInteger )
        assertTrue( A*m == r )
      }
    }
  }

  @Test
  def testWithRandomReal() = {
    val tol = 1e-7
    def f(m : Int, n : Int) = Real(scala.util.Random.nextInt(50))/Real(scala.util.Random.nextInt(50)+1)
    for( N <- List( 4, 5, 10, 20, 50) ) {
    //val N = 5 
      val A = RealMatrix(f,N,N).backwitharray
      if(!(new LU(A).isSingular)){ //only run test if this random matrix is not singular
        val (r, m) = LLL(A)
        val (b,u) = r.orthogonalise
        //println(r)
        //println(m)
        //println(m.det)
        //   println(b.t*b)
        //   println(u)
        //   println( A*m )
        //   println
        //   println
        assertTrue( Hermite.isReduced(r) ) //assert r is Hermitee reduced
        assertTrue( LLL.isReduced(r, Real(0.75)) ) //assert r is LLL reduced
        assertFalse( (m.det.norm - Real.one).normlarger(tol) )
        for( i <- m.indices ) assertTrue( m(i).isInteger(tol) )
        val D = A*m - r
        for(m <- 0 until N) for(n <- 0 until N) assertFalse(D(m,n).normlarger(tol)) //assert columns of B are orthogonal
      }
    }
  }

  
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
