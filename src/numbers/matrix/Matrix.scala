/*
 * @author Robby McKilliam
 */

package numbers.matrix

import numbers.Ring
import numbers.Field
import numbers.EuclideanDomain
import scala.collection.mutable.ArraySeq

/** Base trait for matrices */
trait Matrix[T,M] extends PartialFunction[(Int,Int),T] {
  /// the number of rows
  val M : Int
  /// the number of columns
  val N : Int
    /// construct a matrix from a function
  def construct(f : (Int,Int) => T, M : Int, N : Int) : M
  /// Returns true if m and n are indices for this matrix
  override def isDefinedAt(m : Int, n : Int) = m >= 0 && m < M && n >=0 && n < N
  /// Returns the transpose of this matrix
  def transpose : M = construct( (m,n) => this(n,m), N, M)
  /// returns the submatrix with rows indexed by ms and columns indexed by ns
  def submatrix(ms : IndexedSeq[Int], ns : IndexedSeq[Int]): M = construct( (m,n) => this(ms(m),ns(n)), ms.size, ns.size)
  /// get the mth row
  def row(m: Int) : M = submatrix( ArraySeq(m), 0 until N)
  /// get the nth column
  def column(n: Int) : M = submatrix( 0 until M, ArraySeq(n) )
  /// Returns a copy of this matrix with elements backed by a scala.collections.mutatble.ArraySeq
  def backwitharray : M = {
    val A = new ArraySeq[T](N*M)
    for( m <- 0 until M; n <- 0 until N ) A(m*N + n) = this(m,n)
    construct( (m,n) => A(m*N+n), M, N )
  }

}

trait MatrixWithElementsFromARing[R <: Ring[R],M <: Matrix[R,M]] extends Matrix[R,M] {
  /// matrix addition
  def +(that: M) : M = construct( (m,n) => this(m,n) + that(m,n), M, N )
  /// scalar addition
  def +(that: R) : M = construct( (m,n) => this(m,n) + that, M, N )
  /// matrix subtraction
  def -(that: M) : M = construct( (m,n) => this(m,n) - that(m,n), M, N )
  /// scalar subtraction
  def -(that : R) : M = construct( (m,n) => this(m,n) - that, M, N )
  /// matrix mulitplication
  def *(that: M) : M = construct( (m,n) => (0 until N).foldLeft(this(0,0).zero)( (v, i) => v + this(m,i)*that(i,n)), M, that.N ).backwitharray
  /// scalar mulitplication
  def *(that: R) : M = construct( (m,n) => this(m,n) * that, M, N )
}

trait MatrixWithElementsFromAEuclideanDomain[E <: EuclideanDomain[E,_],M <: MatrixWithElementsFromARing[E,M]] extends Matrix[E,M] {
  /// Compute the Hermite Normal Form
  def hermiteNormalForm : M
  /// Compute the Smith normal form
  def smithNormalForm : M
}

trait MatrixWithElementsFromAField[F <: Field[F,_],M <: MatrixWithElementsFromAEuclideanDomain[F,M]] extends MatrixWithElementsFromAEuclideanDomain[F,M] {
  /// scalar division
  def /(that: F) : M = construct( (m,n) => this(m,n) / that, M, N )
}
