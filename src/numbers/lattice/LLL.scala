/*
 * Various LLL algorithms.
 * @author Robby McKilliam 
 */

package numbers.lattice

import numbers.Field
import numbers.Rational
import numbers.RationalMatrix
import numbers.finite.Real
import numbers.finite.RealMatrix
import numbers.matrix.MatrixWithElementsFromAField

object LLL {
  
}

/** 
 * Abstract class for the LLL decomposition given basis contained in a matrix.
 * Basis vectors are the columns.  Based on Algorithm 2.6.3 of:
 * 
 * Cohen, H. "A course in computational number theory", Springer-Verlag, 1993
 * 
 * Three abstract functions: 
 * def to_matrix(b): converts at Seq[Seq[Field]] to a Matrix[Field]
 * def round(x): that returns the closest integer
 * val half: the number 1/2 between 0 and 1
 * 
 * The argument c must be in the open interval (1/4, 1). 3/4 is the original LLL and 1/2 is
 * Seigel's LLL. Cohen mentions that c makes little differences in practice and that one should
 * perhaps choose c = 0.99.
 */
abstract class LLL[F <: Field[F,_],M <: MatrixWithElementsFromAField[F,M]](basis : M, c : F) {
  
  val n = basis.numCols //number of basis vectors (dimension of the lattice)
  val m = basis.numRows
  val fzero = basis(0,0) //zero value for this field.
  
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
    val D = ((c - u(k)(k-1)*u(k)(k-1))*B(k-1))
    if( D.normlarger(B(k)) ){ //does not satisfy LLL condition
      swap(k)
      k = Math.max(2,k-1)
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
      if( B(k) == fzero ) throw new RuntimeException("Basis is not full rank")
    }
  
  private def red(k : Int, l : Int) {
    if( u(k)(l).normlarger(half) ) {
      val q = round(u(k)(l))
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
  protected def dot(x : Seq[F], y : Seq[F]) = (0 until m).foldLeft(fzero) ( (s, i) => s + x(i)*y(i) ) 
 
  /// Return the LLL recuded basis
  def reducedBasis = toMatrix(b).transpose
  
  /// Return the unimodular transformation matrix H such that BM is the reduces basis.
  def unimodularTransformation = toMatrix(H).transpose
  
  def toMatrix(b : Seq[Seq[F]]) : M
  def round(x : F) : F
  def half : F
  
}

/** 
 * Compute the LLL reduced basis given a matrix with Rational entries.  Exact result guaranteed.
 * Defaults to the original LLL with c = 3/4. 
 */
class RationalLLL(basis : RationalMatrix, c : Rational = Rational(3,4)) extends LLL[Rational, RationalMatrix](basis, c) {
    override def toMatrix(b : Seq[Seq[Rational]]) = RationalMatrix(b)
    override def round(x : Rational) : Rational = x.round
    override def half = Rational(1,2)
}

/** 
 *  Compute the LLL reduced basis given a matrix with number.finite.Real entries (i.e. double 
 *  precison floating point). Defaults to the original LLL with c = 3/4. 
 */
class RealLLL(basis : RealMatrix, c : Real = 0.75) extends LLL[Real, RealMatrix](basis, c) {
    override def toMatrix(b : Seq[Seq[Real]]) = RealMatrix(b)
    override def round(x : Real) : Real= Real(x.d.round)
    override def half = Real(0.5)
}
