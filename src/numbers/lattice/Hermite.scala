/*
 * Hermite reduced basis of lattice
 */

package numbers.lattice

import numbers.RealandRational
import numbers.matrix.MatrixWithElementsFromAField
import scala.collection.mutable.ArraySeq

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
 
 /// Hermite reduce the ith row of basis b.  Mutates basis b, and unimodular transform H, and projection coefficients U.
 def increment[F <: RealandRational[F]](i : Int, b : Seq[ArraySeq[F]], H : Seq[ArraySeq[F]], U : Seq[ArraySeq[F]]) : Unit = {
    val m = b(0).length 
    val n = H.length 
    for( j <- i+1 until n) {
      val q = U(j)(i).round
      for( t <- 0 to i ) U(j)(t) = U(j)(t) - q*U(i)(t)
      for( t <- 0 until m ) b(j)(t) = b(j)(t) - q*b(i)(t)
      for( t <- 0 until n ) H(j)(t) = H(j)(t) - q*H(i)(t)
    }
 }
 
}

/** 
 * Hermite reduction of a lattice given basis matrix.  Basis vectors are the columns.
 */
class Hermite[F <: RealandRational[F],M <: MatrixWithElementsFromAField[F,M]](basis : MatrixWithElementsFromAField[F,M]) {
 
  val n = basis.N //dimension of this lattice
  val m = basis.M //length of vectors in the lattice
  
  protected val (bstar,u) = basis.orthogonalise //get the Gram-Schmith orthogonalised basis
  protected val b = basis.transpose.toArray //memory for new Hermite reduced basis
  protected val H = basis.identity(n).toArray //memory for unimodular transformation
  protected val U = u.transpose.toArray
  
  for( i <- n-2 to 0 by -1 ) Hermite.increment(i,b,H,U)
  
  /// Return the Hermite recuded basis
  def reducedBasis = basis.construct((m,n) => b(n)(m), m, n)
  
  /// Return the unimodular transformation matrix H such that BM is the reduces basis.
  def unimodularTransformation = basis.construct((m,n) => H(n)(m), n, n)

}