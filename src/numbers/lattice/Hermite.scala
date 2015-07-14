/*
 * Hermite reduced basis of lattice
 */

package numbers.lattice

import numbers.RealandRational
import numbers.matrix.MatrixWithElementsFromAField

object Hermite {
 /** 
 * Hermite reduction of an n dimensional lattice given m x n basis matrix.  Basis vectors are the 
 * columns.  Returns (b, u) where b is an m x n reduced basis for the lattice and and u is a 
 * unimodular transformation matrix such that b =  A*u
 */
 def apply[F <: RealandRational[F],M <: MatrixWithElementsFromAField[F,M]](basis : MatrixWithElementsFromAField[F,M]) : (M, M) = {
   val hermite = new Hermite(basis)
   return (hermite.reducedBasis, hermite.unimodularTransformation)
 }
 
  /** Returns true if the columns of this matrix generate a lattice that is Hermite reduced, otherwise false. */
 def isReduced[F <: RealandRational[F],M <: MatrixWithElementsFromAField[F,M]](basis : MatrixWithElementsFromAField[F,M]) : Boolean = {
    val (bstar,u) = basis.orthogonalise //get the Gram-Schmith orthogonalised basis
    val N = u.N
    for( m <- 0 until N) for( n <- m+1 until N) if( u(m,n).normlarger(u(m,n).half) ) return false
    return true
 }
}

/** 
 * Hermite reduction of a lattice given basis matrix.  Basis vectors are the columns.
 */
class Hermite[F <: RealandRational[F],M <: MatrixWithElementsFromAField[F,M]](basis : MatrixWithElementsFromAField[F,M]) {
 
  val n = basis.N //dimension of this lattice
  val m = basis.M //length of vectors in the lattice
  
  protected val (bstar,u) = basis.orthogonalise //get the Gram-Schmith orthogonalised basis
  protected val b = basis.toArray //memory for new Hermite reduced basis
  protected val H = basis.identity(n).toArray //memory for unimodular transformation
  protected val U = u.toArray
  
  for( i <- n-2 to 0 by -1 ) {
    for( j <- i+1 until n) {
      val q = U(i)(j).round
      for( t <- 0 to i ) U(t)(j) = U(t)(j) - q*U(t)(i)
      for( t <- 0 until m ) b(t)(j) = b(t)(j) - q*b(t)(i)
      for( t <- 0 until n ) H(t)(j) = H(t)(j) - q*H(t)(i)
    }
  }
  
  /// Return the Hermite recuded basis
  def reducedBasis = basis.construct((m,n) => b(m)(n), m, n)
  
  /// Return the unimodular transformation matrix H such that BM is the reduces basis.
  def unimodularTransformation = basis.construct((m,n) => H(m)(n), n, n)

}