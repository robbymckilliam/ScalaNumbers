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
  def apply[F <: RealandRational[F],M <: MatrixWithElementsFromAField[F,M]](basis : MatrixWithElementsFromAField[F,M], c : F, zerotol : F) : (M, M) = {
    val lll = new LLL(basis, c, zerotol)
    return (lll.reducedBasis, lll.unimodularTransformation)
  }
  /// Default LLL uses c = 0.75 and Rational.zero for tolerance 
  def apply(M : RationalMatrix, c : Rational) : (RationalMatrix, RationalMatrix) = LLL(M, c, Rational.zero)
  def apply(M : RationalMatrix) : (RationalMatrix, RationalMatrix) = LLL(M,Rational(3,4))
  /// Default LLL uses c = 0.75 and 1e-7 for tolerance
  def apply(M : RealMatrix, c : Real) : (RealMatrix, RealMatrix) = LLL(M, c, Real(1e-10))
  def apply(M : RealMatrix) : (RealMatrix, RealMatrix) = LLL(M, Real(0.75), Real(1e-10))
  
 /** Returns true if the columns of this matrix generate a lattice that is Hermite reduced, otherwise false. */
 def isReduced[F <: RealandRational[F],M <: MatrixWithElementsFromAField[F,M]](basis : MatrixWithElementsFromAField[F,M], c : F, zerotol : F) : Boolean = {
   val (bstar,u) = basis.orthogonalise(zerotol) //get the Gram-Schmith orthogonalised basis
   val N = u.N
   val M = bstar.M
   val zero = basis(0,0).zero
   val half = basis(0,0).half
   for( m <- 0 until N) for( n <- m+1 until N) if( u(m,n).normlarger(half) ) return false //check Hermite reduced
   for(n <- 1 until N) { //Check Lovas condition
     val bn = (0 until M).foldLeft(zero)( (s, i) => s + bstar(i,n)*bstar(i,n) )
     val bn1 = (0 until M).foldLeft(zero)( (s, i) => s + bstar(i,n-1)*bstar(i,n-1) )
     val D = (c - u(n-1,n)*u(n-1,n))*bn1
     if( D.normlarger(bn) ) return false
   }
   return true
 }
 def isReduced[F <: RealandRational[F],M <: MatrixWithElementsFromAField[F,M]](basis : MatrixWithElementsFromAField[F,M], c : F) : Boolean = {
   return isReduced(basis, c, basis(0,0).zero)
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
 * 
 * During the Gram Schmidt process vectors with norm less than or equal to zerotol will be 
 * considered to be the zero vector. This needs to be set to some small positive number for
 * finite precision classes such as Real, but can be set to zero for infinite precision classes
 * such as Rational.
 * 
 */
class LLL[F <: RealandRational[F],M <: MatrixWithElementsFromAField[F,M]](val basis : MatrixWithElementsFromAField[F,M], val c : F, val zerotol : F) {
  
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
    GramSchmidt.increment(k,b,bstar,u, zerotol) //run Gram-Schmit on kth row
    Hermite.increment(k,b,H,u) //Hermite reduce the kth row
    
    //check Lovas condition
    if( lovasCondition(k) ) return reduce(k+1)
    else { //Lovas condition not satisfied
      {val t = b(k); b(k) = b(k-1); b(k-1) = t} //swap basis vectors
      {val t = H(k); H(k) = H(k-1); H(k-1) = t} //swap unimodular transformation vectors
      return reduce(k-1)
    }
      
  }
  
  /// Returns true if the Lovas condition for row k is satisfied
  protected final def lovasCondition(k : Int) : Boolean = {
    if(k == 0) return true; // 1 dimensional lattice always LLL reduced
    val Bk = GramSchmidt.dot(bstar(k),bstar(k))
    //if(Bk == Bk.zero) throw new RuntimeException("Not a valid lattice basis.")
    val Bkm1 = GramSchmidt.dot(bstar(k-1),bstar(k-1))
    val D = (c - u(k)(k-1)*u(k)(k-1))*Bkm1
    return !D.normlarger(Bk) 
  }
 
  /// Return the LLL recuded basis
  def reducedBasis = basis.construct((m,n) => b(n)(m), m, n)
  
  /// Return the unimodular transformation matrix H such that BM is the reduces basis.
  def unimodularTransformation = basis.construct((m,n) => H(n)(m), n, n)

}
