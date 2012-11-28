/*
 * @author Robby McKilliam
 */

package numbers.matrix

import numbers.Ring
import numbers.Field
import numbers.EuclideanDomain
import scala.collection.mutable.ArraySeq

/** Base trait for matrices */
trait Matrix[T,B] extends PartialFunction[(Int,Int),T] {
  /// the number of rows
  val M : Int
  /// the number of columns
  val N : Int
    /// construct a matrix from a function
  def construct(f : (Int,Int) => T, M : Int, N : Int) : B
  /// Returns true if mn = (m,n) are indices for this matrix
  override def isDefinedAt(mn : (Int,Int)) = mn._1 >= 0 && mn._1 < M && mn._2 >=0 && mn._2 < N
  /// Returns the transpose of this matrix
  def transpose : B = construct( (m,n) => this(n,m), N, M)
  /// returns the submatrix with rows indexed by ms and columns indexed by ns
  def submatrix(ms : IndexedSeq[Int], ns : IndexedSeq[Int]): B = construct( (m,n) => this(ms(m),ns(n)), ms.size, ns.size)
  /// get the mth row
  def row(m: Int) : B = submatrix( ArraySeq(m), 0 until N)
  /// get the nth column
  def column(n: Int) : B = submatrix( 0 until M, ArraySeq(n) )
  /// Returns a copy of this matrix with elements backed by a scala.collections.mutatble.ArraySeq
  def backwitharray : B = {
    val A = new ArraySeq[T](N*M)
    for( m <- 0 until M; n <- 0 until N ) A(m*N + n) = this(m,n)
    construct( (m,n) => A(m*N+n), M, N )
  }

}

trait MatrixWithElementsFromARing[R <: Ring[R],B <: Matrix[R,B]] extends Matrix[R,B] {
  /// matrix addition
  def +(that: B) : B = construct( (m,n) => this(m,n) + that(m,n), M, N )
  /// scalar addition
  def +(that: R) : B = construct( (m,n) => this(m,n) + that, M, N )
  /// matrix subtraction
  def -(that: B) : B = construct( (m,n) => this(m,n) - that(m,n), M, N )
  /// scalar subtraction
  def -(that : R) : B = construct( (m,n) => this(m,n) - that, M, N )
  /// matrix mulitplication
  def *(that: B) : B = construct( (m,n) => (0 until N).foldLeft(this(0,0).zero)( (v, i) => v + this(m,i)*that(i,n)), M, that.N ).backwitharray
  /// scalar mulitplication
  def *(that: R) : B = construct( (m,n) => this(m,n) * that, M, N )
}

trait MatrixWithElementsFromAEuclideanDomain[E <: EuclideanDomain[E,_],B <: MatrixWithElementsFromARing[E,B]] extends MatrixWithElementsFromARing[E,B] {
  /// Compute the Hermite Normal Form
  def hermiteNormalForm : (B,B)
  def hnf = hermiteNormalForm
  /// Compute the Smith normal form
  def smithNormalForm : (B,B,B)
  def snf = smithNormalForm
}

trait MatrixWithElementsFromAField[F <: Field[F,_],B <: MatrixWithElementsFromAEuclideanDomain[F,B]] extends MatrixWithElementsFromAEuclideanDomain[F,B] {
  /// scalar division
  def /(that: F) : B = construct( (m,n) => this(m,n) / that, M, N )
}
