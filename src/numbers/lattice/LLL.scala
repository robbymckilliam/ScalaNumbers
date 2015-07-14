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
import numbers.matrix.MatrixWithElementsFromAField

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
  protected val B = (0 until n).map( i => dot(b(i),b(i)) ).toBuffer //squared length of basis vectors

  private var k = 1
  private var kmax = 0
  while( k < n ) { 
    if( k > kmax ) incrementalGramSchmidt()
    red(k,k-1)
    //Test LLL condition
    val D = (c - u(k)(k-1)*u(k)(k-1))*B(k-1)
    if( D.normlarger(B(k)) ){ //does not satisfy LLL condition
      swap(k)
      k = Math.max(1,k-1)
    }
    else {
      for( l <- k-2 to 0 by -1) red(k,l)
      k = k+1
    }
  }
  
  private def incrementalGramSchmidt() {
      kmax = k
      for( i <- 0 until m ) bstar(k)(i) = b(k)(i)
      for( j <- 0 to k-1 ) {
        u(k)(j) = dot(b(k),bstar(j))/B(j)
        for( i <- 0 until m ) bstar(k)(i) = bstar(k)(i) - u(k)(j)*bstar(j)(i)
      }
      B(k) = dot(bstar(k),bstar(k))
      if( B(k) == zero ) throw new RuntimeException("Basis is not full rank")
    }
  
  private def red(k : Int, l : Int) {
    if( u(k)(l).normlarger(half) ) {
      val q = u(k)(l).round
      for(i <- 0 until m) b(k)(i) = b(k)(i) - q*b(l)(i)
      for(i <- 0 until n) H(k)(i) = H(k)(i) - q*H(l)(i)
      u(k)(l) = u(k)(l) - q
      for(i <- 0 to l-1) u(k)(i) = u(k)(i) - q*u(l)(i)
    }
  }
  
  private def swap(k : Int) {
    {val t = b(k); b(k) = b(k-1); b(k-1) = t} //swap b(k) and b(k-1)
    {val t = H(k); H(k) = H(k-1); H(k-1) = t} //swap H(k) and H(k-1)
    for( j <- 0 to k-2) { val t = u(k)(j); u(k)(j) = u(k-1)(j); u(k-1)(j) = t} //swap u(k)(j) and u(k-1)(j) for j = 0 ... k-2
    val ut = u(k)(k-1)
    val Bt = B(k) + ut*ut*B(k-1)
    u(k)(k-1) = ut * B(k-1)/Bt 
    val bt = bstar(k-1).clone
    for(i <- 0 until m) bstar(k-1)(i) = b(k)(i) + ut*bt(i)
    for(i <- 0 until m) bstar(k)(i) = -u(k)(k-1)*bstar(k)(i) + (B(k)/Bt)*bt(i)
    B(k) = B(k-1)*B(k)/Bt
    B(k-1) = Bt
    for( i <- k+1 to kmax ) {
      val t = u(i)(k)
      u(i)(k) = u(i)(k-1) - ut*t
      u(i)(k-1) = t + u(k)(k-1)*u(i)(k)
    }
  }
  
  //inner product between vectors
  protected def dot(x : Seq[F], y : Seq[F]) = (0 until m).foldLeft(zero) ( (s, i) => s + x(i)*y(i) ) 
 
  /// Return the LLL recuded basis
  def reducedBasis = basis.construct((m,n) => b(n)(m), m, n)
  
  /// Return the unimodular transformation matrix H such that BM is the reduces basis.
  def unimodularTransformation = basis.construct((m,n) => H(n)(m), n, n)

}