/*
 * Various LLL algorithms.
 * @author Robby McKilliam 
 */

package numbers.lattice

import numbers.Rational
import numbers.RationalMatrix
import numbers.RealandRational
import numbers.finite.Real
import numbers.finite.RealMatrix
import numbers.matrix.GramSchmidt
import numbers.matrix.MatrixWithElementsFromAField
import scala.annotation.tailrec

object LLL {
  def apply[F <: RealandRational[F],M <: MatrixWithElementsFromAField[F,M]](basis : MatrixWithElementsFromAField[F,M], c : F) : (M, M) = {
    val lll = new LLL(basis, c)
    return (lll.reducedBasis, lll.unimodularTransformation)
  }
  /// Default LLL uses c = 0.75 
  def apply(M : RationalMatrix) : (RationalMatrix, RationalMatrix) = LLL(M, Rational(3,4))
  /// Default LLL uses c = 0.75
  def apply(M : RealMatrix) : (RealMatrix, RealMatrix) = LLL(M, Real(0.75))
  
 /** Returns true if the columns of this matrix generate a lattice that is Hermite reduced, otherwise false. */
 def isReduced[F <: RealandRational[F],M <: MatrixWithElementsFromAField[F,M]](basis : MatrixWithElementsFromAField[F,M], c : F) : Boolean = {
    val (bstar,u) = basis.orthogonalise //get the Gram-Schmith orthogonalised basis
    val N = u.N
    val M = bstar.M
    for( m <- 0 until N) for( n <- m+1 until N) if( u(m,n).normlarger(u(m,n).half) ) return false //check Hermite reduced
    for(n <- 1 until N) { //Check Lovas condition
      val bn = (0 until M).map(i=>bstar(n,i)).reduceLeft( (s, v) => s + v*v ) 
      val bn1 = (0 until M).map(i=>bstar(n-1,i)).reduceLeft( (s, v) => s + v*v ) 
      val D = (c - u(n-1,n)*u(n-1,n))*bn1
      if( D.normlarger(bn) ) return false
    }
    return true
 }
 
}

/** 
 * Abstract class for the LLL decomposition given basis contained in a matrix.
 * Basis vectors are the columns.  Based on Algorithm 2.6.3 of:
 * 
 * Cohen, H. "A course in computational number theory", Springer-Verlag, 1993
 * 
 * The argument c must be in the open interval (1/4, 1). 3/4 is the original LLL and 1/2 is
 * Seigel's LLL. Cohen mentions that c makes little differences in practice and that one should
 * perhaps choose c = 0.99.
 */
class LLL[F <: RealandRational[F],M <: MatrixWithElementsFromAField[F,M]](basis : MatrixWithElementsFromAField[F,M], c : F) {
  
  val n = basis.numCols //number of basis vectors (dimension of the lattice)
  val m = basis.numRows
  val zero = basis(0,0).zero //zero value for this field.
  val half = basis(0,0).half
  
  protected val b = basis.transpose.toArray //copy elements to mutable structure. Basis vectors are the "rows" in this structure
  protected val bstar = basis.transpose.toArray //stores the Gram-Schmidt vectors
  protected val H = basis.identity(n).toArray //stores the unimodular transformation
  protected val u = basis.identity(n).toArray //memory for Gram-Schmidt coefficients

  //start LLL
  reduce(0)
  
  @tailrec protected final def reduce(k : Int) : Unit = {
    if(k == n) return; // finished
    GramSchmidt.increment(k,b,bstar,u) //run Gram-Schmit on kth row
    println(basis.construct((m,n) => u(m)(n), n, n))
    Hermite.increment(k,b,H,u) //Hermite reduce the kth row
    println(basis.construct((m,n) => u(m)(n), n, n))
    println
    return reduce(k+1)
  }
 
  /// Return the LLL recuded basis
  def reducedBasis = basis.construct((m,n) => b(n)(m), m, n)
  
  /// Return the unimodular transformation matrix H such that BM is the reduces basis.
  def unimodularTransformation = basis.construct((m,n) => H(n)(m), n, n)

}