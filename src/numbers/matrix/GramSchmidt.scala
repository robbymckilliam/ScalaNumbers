/**
 * Gram-Schmidt orthogonalisation applied to the columns of an m by n matrix A. 
 * Returns an m by n matrix B with orthogonal (not necessarily orthonormal columns) and 
 * and n by n upper triangular matrix U such that A = BU. 
 * 
 * The advantage of not orthonormalising is that only addition, multiplication, and division are 
 * required and so B and U will have entries from the same field as A. See Proposition 2.5.4 of:
 * 
 * Cohen, H. "A course in computational number theory", Springer-Verlag, 1993
 * 
 */
package numbers.matrix

import numbers.Field
import scala.collection.mutable.ArraySeq

object GramSchmidt {
  
  def apply[F <: Field[F,_],Matrix <: MatrixWithElementsFromAField[F,Matrix]](A : MatrixWithElementsFromAField[F,Matrix], zerotol: F) : (Matrix, Matrix) = {
    val gs = new GramSchmidt(A,zerotol)
    return (gs.B, gs.U)
  }
  
  /** 
   *Default zero tolerance is this field zero element. This will work fine for infinite precision
   *classes like Rational, but will be numerically unstable for large matrices and finite precision
   *classes like Real and Complex
   */
  def apply[F <: Field[F,_],Matrix <: MatrixWithElementsFromAField[F,Matrix]](A : MatrixWithElementsFromAField[F,Matrix]) : (Matrix, Matrix) = {
    return GramSchmidt(A, A(0,0).zero)
  }
  
  /** 
   * Projects x orthogonally to p and returns result in y.  Assumes memory for y is allocated.
   * Throws exception if it's not.  Returns the projection coefficient dot(x,v)/dot(v,v).
   * If p is the vector of zeros then y = x
   */
  def project[F <: Field[F,_]](x : Seq[F], p : Seq[F], y : ArraySeq[F], zerotol : F) : F = {
    val m = x.length
    if( y.length != m ) throw new ArrayIndexOutOfBoundsException("x, v, and y must have the same length") 
    val normp = dot(p,p)
    val u = if( normp.normlarger(zerotol) ) dot(x,p)/normp else normp.zero 
    for(i <- 0 until m) y(i) = x(i) - u*p(i)
    return u //the projection coefficent
  }
  
  /// inner product between vectors
  def dot[F <: Field[F,_]](x : Seq[F], y : Seq[F]) : F = {
      val m = x.length
      if( m == 0 || y.length != m ) throw new ArrayIndexOutOfBoundsException("Do products of vectors must be the same length")
      return (0 until m).foldLeft(x(0).zero) ( (s, i) => s + x(i)*y(i) ) 
  }
  
  /// Run Gram-Schmit on nth row of b and store in bstar. Write projection coefficient to u
  def increment[F <: Field[F,_]](n : Int, b : Seq[Seq[F]], bstar : Seq[ArraySeq[F]], u : Seq[ArraySeq[F]], zerotol : F) : Unit = {
    for( i <- b(0).indices ) bstar(n)(i) = b(n)(i)
    for( m <- 0 until n ) {
      u(n)(m) = GramSchmidt.project(bstar(n),bstar(m),bstar(n), zerotol)
    }
  }                           

}

/** 
 *Compute the Gram Schmidt orthogonalisation of matrix A.  Vectors with norm less than or equal to zerotol 
 *are considered to be zero.  This can set to zero for infinite precision data types such as Rational, but needs
 *to be carefully selected for finite precision data types such as Real and Complex.
 */
class GramSchmidt[F <: Field[F,_],Matrix <: MatrixWithElementsFromAField[F,Matrix]](val A : MatrixWithElementsFromAField[F,Matrix], val zerotol: F) {
  
  val N = A.N //number of columns
  val M = A.M //number of rows
  
  private val b = A.transpose.toArray //get memory containing transpose of A. Will operate "rowise" on b
  private val u = A.identity(N).toArray //memory of the U matrix)
  
  // b(0) is already the first column of A so this loop could start at 0
  // but starting at zero as a test case (since LLL code will do this, for example)
  for( n <- 0 until N ) GramSchmidt.increment(n,b,b,u, zerotol)
  
  /// The m x n matrix of orthogonal vectors B
  val B = A.construct( (m,n) => b(n)(m), M, N)
  /// The n x n upper triangular matrix u
  val U = A.construct( (m,n) => u(n)(m), N, N)
  
}
