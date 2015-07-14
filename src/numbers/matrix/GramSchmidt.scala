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
  
  /** 
   * Projects x orthogonally to p and returns result in y.  Assumes memory for y is allocated.
   * Throws exception if it's not.  Returns the projection coefficient dot(x,v)/dot(v,v).
   * If p is the vector of zeros then y = x
   */
  def project[F <: Field[F,_]](x : Seq[F], p : Seq[F], y : ArraySeq[F]) : F = {
    val m = x.length
    if( y.length != m ) throw new ArrayIndexOutOfBoundsException("x, v, and y must have the same length") 
    val normp = dot(p,p)
    val u = if( normp != normp.zero) dot(x,p)/normp else normp.zero
    for(i <- 0 until m) y(i) = x(i) - u*p(i)
    return u //the projection coefficent
  }
  
  /// inner product between vectors
  def dot[F <: Field[F,_]](x : Seq[F], y : Seq[F]) : F = {
      val m = x.length
      if( m == 0 || y.length != m ) throw new ArrayIndexOutOfBoundsException("Do products of vectors must be the same length")
      return (0 until m).foldLeft(x(0).zero) ( (s, i) => s + x(i)*y(i) ) 
  }

}

class GramSchmidt[F <: Field[F,_],Matrix <: MatrixWithElementsFromAField[F,Matrix]](val A : MatrixWithElementsFromAField[F,Matrix]) {
  
  val N = A.N //number of columns
  val M = A.M //number of rows
  
  private val b = A.transpose.toArray //get memory containing transpose of A. Will operate "rowise" on b
  private val u = A.identity(N).toArray //memory of the U matrix)
  
  // b(0) is already the first column of A so start at the second
  for( n <- 1 until N ) {
    for( m <- 0 until n ) {
      u(m)(n) = GramSchmidt.project(b(n),b(m),b(n))
    }
  }
  
  /// The m x n matrix of orthogonal vectors B
  val B = A.construct( (m,n) => b(n)(m), M, N)
  /// The n x n upper triangular matrix u
  val U = A.construct( (m,n) => u(m)(n), N, N)
  
}
