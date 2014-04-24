/*
 * @author Robby McKilliam
 */

package numbers.matrix

import numbers.Field

/** 
 * Computes the LU decomposition of this matrix.
 * Computes permutation matrix p, lower triangular matrix l and upper triangular matrix u
 * such that the product pA = lu
 */
class LU[F <: Field[F,N], N <: Ordered[N],B <: MatrixWithElementsFromAField[F,B]](val A : MatrixWithElementsFromAField[F,B]) {
  
  val M = A.M
  val N = A.N
  if(M < N) throw new ArrayIndexOutOfBoundsException("number of columns cannot exceed number of rows for LU. You might want to transponse your matrix first.")
  
  val one = A(0,0).one
  val zero = A(0,0).zero
  
  // This implementation is ported from the Jama Matrix library
  // Initialize.
  private val LU = A.toArray
  private val piv = (0 until M).toArray; //store the pivot vecotor
  private var pivsign = 1;
  // Main loop.
  for(k <- 0 until N) {
    // Find pivot.
    var p = k;
    for (i <- k+1 until M) {
      if( LU(i)(k).norm > LU(p)(k).norm ) p = i 
    }
    // Exchange if necessary.
    if (p != k) {
      for (j <- 0 until N ) {
        val t = LU(p)(j); LU(p)(j) = LU(k)(j); LU(k)(j) = t
      }
      val t = piv(p); piv(p) = piv(k); piv(k) = t;
      pivsign = -pivsign;
    }
    // Compute multipliers and eliminate k-th column.
    if ( LU(k)(k) != zero ) {
      for(i <- k+1 until M) {
        LU(i)(k) = LU(i)(k)/LU(k)(k);
        for( j <- k+1 until A.N) LU(i)(j) = LU(i)(j) - LU(i)(k)*LU(k)(j)
      }
    }
  }
  
  //the permutation matrix
  val P = A.construct( (m,n) => if(piv(m)==n) one else zero, M, M)
  //L is the lower triangular part of LU
  val L = A.construct( (m,n) => if(m > n) LU(m)(n) else if(m==n) one else zero, M, N)
  //U is the upper triangular part of LU 
  val U = A.construct( (m,n) => if(m <= n) LU(m)(n) else zero, N, N)
  
  /// This is the sign of the pivot, i.e., the determinant of the permutation matrix P
  val pivot_sign = pivsign
  
  /// The permutation vector
  val permutation : Seq[Int] = piv
  
  /// Returns true if this matrix is singular 
  def isSingular = (0 until N).exists(k=>LU(k)(k) == zero)
  
}
