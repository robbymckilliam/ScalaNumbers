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

class LUTest {

  @Test
  def realRandomLUTest() {
    val sizes = List( (5,5), (5,4) )
    for( (m_, n_) <- sizes ){
      def f(m : Int, n : Int) = Real(scala.util.Random.nextGaussian)
      val A = new RealMatrix(f,m_,n_).backwitharray
      val (l, u, p) = A.lu
      assertTrue( (p*A - l*u).frobeniusNorm < 1e-7 )
    }
  }
  
  @Test
  def complexRandomLUTest() {
    val sizes = List( (5,5), (5,4) )
    for( (m_, n_) <- sizes ){
      def f(m : Int, n : Int) = RectComplex(scala.util.Random.nextGaussian, scala.util.Random.nextGaussian)
      val A = new ComplexMatrix(f,m_,n_).backwitharray
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
      val A = new RealMatrix(f,M,N).backwitharray
      assertFalse( new numbers.matrix.LU[Real,Real,RealMatrix](A).isSingular )
    }
    {
      val M = 5
      val N = 5
      def f(m : Int, n : Int) = Real.one
      val A = new RealMatrix(f,M,N)
      assertTrue( new numbers.matrix.LU[Real,Real,RealMatrix](A).isSingular )
    }
  }

}
