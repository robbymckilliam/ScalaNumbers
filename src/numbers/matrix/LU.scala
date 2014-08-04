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
class LU[F <: Field[F,_],B <: MatrixWithElementsFromAField[F,B]](val A : MatrixWithElementsFromAField[F,B]) {
  
  val M = A.M //number of rows
  val N = A.N //number of columns
  if(M < N) throw new ArrayIndexOutOfBoundsException("Number of columns cannot exceed number of rows for LU. You might want to transponse your matrix first.")
  
  val one = A(0,0).one
  val zero = A(0,0).zero
  
  // This implementation is ported from the Jama Matrix library
  // Initialize.
  private val LU = A.toArray
  private val piv = (0 until M).toArray; //store the pivot vector
  private var pivsign = one;
  // Main loop.
  for(k <- 0 until N) {
    // Find pivot.
    var p = k;
    for (i <- k+1 until M) {
      if( LU(i)(k).normlarger(LU(p)(k)) ) p = i 
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
        for( j <- k+1 until N) LU(i)(j) = LU(i)(j) - LU(i)(k)*LU(k)(j)
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
  val permutation : IndexedSeq[Int] = piv
  
  /// Returns true if this matrix is singular 
  def isSingular = (0 until N).exists(k=>LU(k)(k) == zero)
  
  /** Solve A*X = D.
   @param  D   A Matrix with as many rows as A matix and any number of columns.
   @return     X so that L*U*X = B(piv,:)
   @exception  IllegalArgumentException Matrix row dimensions must agree.
   @exception  RuntimeException  Matrix is singular.
   */
   def solve(C : B) : B = {
      if (C.M != M) throw new IllegalArgumentException("Matrix row dimensions must agree.");
      if (isSingular) throw new RuntimeException("Matrix is singular.");

      // Copy right hand side with pivoting into a mutable structure
      val X = C.submatrix(piv, 0 until C.N).toArray; 

      // Solve L*Y = B(piv,:)
      for (k <- 0 until N) 
         for ( i <- k+1 until N) 
            for ( j <- 0 until C.N ) 
               X(i)(j) -= X(k)(j)*LU(i)(k);
               
      // Solve U*X = Y;
      for (k <- N-1 to 0 by -1) {
         for (j <- 0 until C.N) {
            X(k)(j) /= LU(k)(k);
         }
         for (i <- 0 until k) 
            for (j <- 0 until C.N) 
               X(i)(j) -= X(k)(j)*LU(i)(k);
      }
      
      return A.construct((m,n) => X(m)(n), piv.length, C.N);
   }
  
}
