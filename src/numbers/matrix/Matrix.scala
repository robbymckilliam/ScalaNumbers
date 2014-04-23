/*
 * @author Robby McKilliam
 */

package numbers.matrix

import numbers.Group
import numbers.Ring
import numbers.RingWithUnity
import numbers.EuclideanDomain
import numbers.Field
import scala.collection.mutable.ArraySeq

/** Base trait for matrices */
trait Matrix[T,B] extends PartialFunction[(Int,Int),T] {
  /// the number of rows
  val M : Int
  /// the number of columns
  val N : Int
  /// construct a matrix from a function
  def construct(f : (Int,Int) => T, M : Int, N : Int) : B
  /// construct a row vector from a function
  def constructRow(f : (Int) => T, N : Int) = construct( (m,n) => f(n), 1, N)
  /// construct a column vector from a function
  def constructColumn(f : (Int) => T, M : Int) = construct( (m,n) => f(m), M, 1)
  /// Returns true if mn = (m,n) are indices for this matrix
  override def isDefinedAt(mn : (Int,Int)) = mn._1 >= 0 && mn._1 < M && mn._2 >=0 && mn._2 < N
  def isScalar = (N==1) && (M ==1)
  def isRow = (M==1)
  def isColumn = (N==1)
    /// Get the element on the mth row and nth column of this matrix.  This must be overriden by the inheriting class
  protected def get(m : Int, n : Int) : T
  override def apply(mn : (Int,Int)) : T = {
    if(!isDefinedAt(mn)) throw new ArrayIndexOutOfBoundsException("You attempted to index an " + M + " x " + N + " matrix at " + mn)
    return get(mn._1,mn._2)
  }
  def apply(n : Int) : T = {
    if(isRow) return this(0,n)
    else if(isColumn) return this(n,0)
    else throw new ArrayIndexOutOfBoundsException("You attempted to index an " + M + " x " + N + " as if it were a column or row vector")
  }
  /// Returns a collection of tuple that index all of elements in this matrix (row wise)
  def indices = for( m<-(0 until M).view; n<-(0 until N).view ) yield (m,n)
  /// Returns the transpose of this matrix
  def transpose : B = construct( (m,n) => this(n,m), N, M )
  /// Returns the transpose of this matrix
  def t = transpose 
  /// returns the submatrix with rows indexed by ms and columns indexed by ns
  def submatrix(ms : IndexedSeq[Int], ns : IndexedSeq[Int]): B = construct( (m,n) => this(ms(m),ns(n)), ms.size, ns.size)
  /// get the mth row
  def row(m: Int) : B = submatrix( ArraySeq(m), 0 until N)
  /// get the nth column
  def column(n: Int) : B = submatrix( 0 until M, ArraySeq(n) )
  /// Returns a copy of this matrix with elements backed by a scala.collections.mutatble.ArraySeq
  def backwitharray : B = {
    val A = toSeq
    construct( (m,n) => A(m*N+n), M, N )
  }
  /// Returns an ArraySeq with the elements of this array (m,n)th element in matrix corresponds with m*N+n th element in the array.
  def toSeq : ArraySeq[T] = {
    val A = new ArraySeq[T](N*M)
    for( m <- 0 until M; n <- 0 until N ) A(m*N + n) = this(m,n)
    A
  }
  override def toString : String  = (0 until M).foldLeft(""){ 
    (sr, m) => sr + (1 until N).foldLeft(this(m,0).toString)( (sc, n) => sc + " " + this(m,n).toString) + "\n" 
  }
}

trait MatrixWithElementsFromAGroup[G <: Group[G],B <: MatrixWithElementsFromAGroup[G,B]] extends Matrix[G,B] {
  /// matrix addition
  def +(that: B) : B = construct( (m,n) => this(m,n) + that(m,n), M, N )
  /// scalar addition
  def +(that: G) : B = construct( (m,n) => this(m,n) + that, M, N )
  /// matrix subtraction
  def -(that: B) : B = construct( (m,n) => this(m,n) - that(m,n), M, N )
  /// scalar subtraction
  def -(that : G) : B = construct( (m,n) => this(m,n) - that, M, N )
  /// Negative of this matrix (additive inverse)
  def unary_- : B = construct( (m,n) => -this(m,n), M, N )
  /// matrix of zeros
  def zeros(M : Int, N : Int) : B = construct( (m,n) => this(0,0).zero, M,N)
  def zeros(N : Int) : B = zeros(N,N)
  /// return true if this matrix is upper triangular (i.e., has zeros on the lower triangular elements)
  def isUpperTriangular : Boolean = {
    var isupper = true
    for( m <- 0 until M )
      for( n <- 0 until m) isupper = isupper && this(m,n) == this(0,0).zero
    return isupper
  }
  /// return true if this matrix is lower triangular (i.e., has zeros on the upper triangular elements)
  def isLowerTriangular = this.transpose.isUpperTriangular
  
}

trait MatrixWithElementsFromARing[R <: Ring[R],B <: MatrixWithElementsFromARing[R,B]] extends MatrixWithElementsFromAGroup[R,B] {
  /// matrix mulitplication
  def *(that: B) : B = construct( (m,n) => (0 until N).foldLeft(this(0,0).zero)( (v, i) => v + this(m,i)*that(i,n)), M, that.N ).backwitharray
  //def *(that: B) : B = construct( (m,n) => (0 until N).map(i=>(this(m,i),that(i,n))).map(Function.tupled(_*_)).reduceLeft(_+_), M, that.N).backwitharray //multiply that doesn't require this(0,0).zero
  /// scalar mulitplication
  def *(that: R) : B = construct( (m,n) => this(m,n) * that, M, N )
}

trait MatrixWithElementsFromARingWithUnity[R <: RingWithUnity[R],B <: MatrixWithElementsFromARingWithUnity[R,B]] extends MatrixWithElementsFromARing[R,B] {
  /// contruct an identity matrix
  def identity(M : Int, N : Int) = construct( (m,n) => if(m==n) this(0,0).one else this(0,0).zero, M,N)
  def identity(N : Int) : B = identity(N,N)
  /// the identity matrix
  def ones(M : Int, N : Int) : B = construct( (m,n) => this(0,0).one, M,N)
  def ones(N : Int) : B = ones(N,N)
}

trait MatrixWithElementsFromAEuclideanDomain[E <: EuclideanDomain[E,_],B <: MatrixWithElementsFromAEuclideanDomain[E,B]] extends MatrixWithElementsFromARingWithUnity[E,B] {
  /// Compute the Hermite Normal Form
  def hermiteNormalForm : (B,B)
  def hnf = hermiteNormalForm
  /// Compute the Smith normal form
  def smithNormalForm : (B,B,B)
  def snf = smithNormalForm
}

trait MatrixWithElementsFromAField[F <: Field[F,_],B <: MatrixWithElementsFromAField[F,B]] extends MatrixWithElementsFromAEuclideanDomain[F,B] {
  /// scalar division
  def /(that: F) : B = construct( (m,n) => this(m,n) / that, M, N )
  
  /** 
   * Return the PLU decomposition of this matrix.  The matrix must by square.
   * Returns permutation matrix P, lower triangular matrix L and upper triangular matrix U
   * such that the product PLU is equal to this matrix
   */
  def lu : (B, B, B) = throw new UnsupportedOperationException("not implemented yet")
  
}
