/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package numbers.matrix.field

import numbers.finite.Complex
import numbers.IntegerMatrix
import numbers.Integer
import numbers.finite.ComplexMatrix
import numbers.finite.RectComplex
import org.junit.Test
import org.junit.Assert._


class MatrixTest {

  val tol = 1e-6
  
  def diff(x : Complex, y : Complex) = (x-y).magnitude
  
  @Test
  def complexElementTest() {
    val N = 5
    val M = 4
    def f(m : Int, n : Int) = new RectComplex(n, m)
    val A = new ComplexMatrix(f,M,N)
    for(m <- 0 until M; n <- 0 until N) assertTrue(diff(f(m,n),A(m,n)) <= tol)
  }
  
  @Test
  def integerElementTest() {
    val N = 5
    val M = 4
    def f(m : Int, n : Int) = Integer(n*m)
    val A = new IntegerMatrix(f,M,N)
    for(m <- 0 until M; n <- 0 until N) assertTrue(f(m,n)==A(m,n))
  }
  
}
