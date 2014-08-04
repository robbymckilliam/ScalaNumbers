/*
 * Tests for the LU decomposition
 * @author Robby McKilliam
 */

package numbers.matrix

import org.junit._
import Assert._
import numbers.finite.Real
import numbers.finite.RealMatrix
import numbers.finite.Complex
import numbers.finite.RectComplex
import numbers.finite.ComplexMatrix
import numbers.Rational
import numbers.RationalMatrix

class LUTest {

  @Test
  def realRandomLUTest() {
    val sizes = List( (5,5), (5,4) )
    for( (m_, n_) <- sizes ){
      def f(m : Int, n : Int) = Real(scala.util.Random.nextGaussian)
      val A = RealMatrix(f,m_,n_).backwitharray
      val (l, u, p) = A.lu
      assertTrue( (p*A - l*u).frobeniusNorm < 1e-7 )
    }
  }
  
  @Test
  def rationalRandomLUTest() {
    val sizes = List( (5,5), (5,4) )
    for( (m_, n_) <- sizes ){
      def f(m : Int, n : Int) = Rational(scala.util.Random.nextInt(100), scala.util.Random.nextInt(100)+1)
      val A = RationalMatrix(f,m_,n_).backwitharray
      val (l, u, p) = A.lu
      val pA = p*A
      val LU = l*u
      assertFalse( pA.indices.exists( i => pA(i) != LU(i) ) ) //check all elements are equal
    }
  }
  
  @Test
  def complexRandomLUTest() {
    val sizes = List( (5,5), (5,4) )
    for( (m_, n_) <- sizes ){
      def f(m : Int, n : Int) = RectComplex(scala.util.Random.nextGaussian, scala.util.Random.nextGaussian)
      val A = ComplexMatrix(f,m_,n_).backwitharray
      val (l, u, p) = A.lu
      assertTrue( (p*A - l*u).frobeniusNorm < 1e-7 )
    }
  }
  
  @Test
  def testIsSingular() {
    {
      val M = 30
      val N = 30
      def f(m : Int, n : Int) = Real(scala.util.Random.nextGaussian) //unlikely this will ever be singular!
      val A = RealMatrix(f,M,N).backwitharray
      assertFalse( new numbers.matrix.LU[Real,RealMatrix](A).isSingular )
    }
    {
      val M = 5
      val N = 5
      def f(m : Int, n : Int) = Real.one
      val A = RealMatrix(f,M,N)
      assertTrue( new numbers.matrix.LU[Real,RealMatrix](A).isSingular )
    }
  }
  
  @Test
  def solveTest() {
    val sizes = List( (5,5) )
    for( (m_, n_) <- sizes ){
      def f(m : Int, n : Int) = Real(scala.util.Random.nextGaussian)
      val A = RealMatrix(f,m_,n_).backwitharray
      val C = RealMatrix(f,m_,n_).backwitharray
      val X = new numbers.matrix.LU[Real,RealMatrix](A).solve(C)
      assertTrue( (A*X - C).frobeniusNorm < 1e-7 )
    }
  }

}
