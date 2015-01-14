/*
 * @Robby McKilliam
 */

package numbers.finite

import numbers.Element
import numbers.Field
import numbers.matrix.MatrixWithElementsFromAField
import scala.math.min
import scala.math.max
import scala.math.sqrt
import scala.language.implicitConversions
import scala.annotation.tailrec

/**
 * Provides static definitions of the multiplicative and additive identities.
 */
object Real {
  val one = new Real(1.0) 
  val zero = new Real(0.0)
  
  def apply(x : Double) = new Real(x)
  implicit def toReal(d : Double) = Real(d)
  implicit def toReal(i : Int) = Real(i.toDouble)
  
  ///Computes the rational number with simple continued fraction given by a
  def from_continued_fraction(a : Seq[numbers.Integer]) = new numbers.Algorithms.ContinuedFraction[Real](a.map(i=>Real(i.toDouble))).value
  ///Compute the rational number approximating the given simple infinite continued fraction with accuracy tol (guarateed).
  def from_continued_fraction(a : Int => numbers.Integer, tol : Real, ITRMAX : Int = 10000) = new numbers.Algorithms.InfiniteContinuedFraction[Real](i=>Real(a(i).toDouble), tol, ITRMAX).value
  
}

/**
 *  This is a potential candidate for scala 2.10's value classes.
 */
class Real(val d : Double) extends Field[Real, Real] with Ordered[Real] {
  //@inline final def toDouble = d
  
  final def unary_- = new Real(-d)
  final override def +(that: Real) = new Real(d + that.d)
  final override def -(that: Real) = new Real(d - that.d)
  final override def *(that: Real) = new Real(d * that.d)
  final override def /(that: Real) = new Real(d / that.d)
  final override def reciprocal : Real = one/this
  
  final override def one : Real = Real.one
  final override def zero : Real = Real.zero
    
  final override def norm : Real = new Real(d.abs)
  
  final override def ==(that : Real) = this.d == that.d
      
  /** Uses scala's internal Ordered, only need to override compare */
  final override def compare(that : Real) : Int = this.d.compare(that.d)
  
  final override def toString : String  = d.toString
  
  /** 
   *Returns the continued fraction expansion of this Real.  This is the naive floor and reciprocate
   *algorithm.  As such it suffers from numerical imprecision.  Could be improved by another method
   *of some kind
   *@param tol  optional argument sets how close to an integer is close enough, default 1e-10.
   *@param ITRMAX  optional argument sets the maximum number of terms to compute in the contined fracion, default 1e-13.
   */
  final def continued_fraction(tol : Double = 1e-10, ITRMAX : Int = 100) : Seq[numbers.Integer] = continued_fraction(List[numbers.Integer](),tol, ITRMAX).reverse.toSeq
  @tailrec final protected def continued_fraction(a : List[numbers.Integer], tol : Double, iters_left : Int) : List[numbers.Integer] = {
    if(iters_left <=0) throw new RuntimeException("Maximum number of iterations reached by Real.contined_fraction. You might have tried to set the tolerance too small.")
    val an = d.floor.toInt //next element in the continued fraction
    val rem = d - an
    if(rem < tol) return numbers.Integer(an) :: a 
    val r = 1.0/rem //reciprocal of the positive fractional part of this number
    return Real(r).continued_fraction(numbers.Integer(an) :: a, tol, iters_left-1) //recursively compute continued fraction elements
  }
  
  /** Return true if this Real is less than tol from an integer */
  final def isInteger(tol : Double) = (d - d.round).abs < tol;
  
}


object RealMatrix {
  /// contruct identity matrix
  def identity(M : Int, N : Int) : RealMatrix = new RealMatrix( (m,n) => if(m==n) Real.one else Real.zero, M,N)
  def identity(N : Int): RealMatrix = identity(N,N)
  def apply(f : (Int,Int) => Real, M : Int, N : Int) = construct(f,M,N)
  def apply(a : Seq[Seq[Real]]) : RealMatrix = new RealMatrix((m,n) => a(m)(n),a.length,a(0).length) 
  def fromDoubleArray(a : Seq[Seq[Double]]) : RealMatrix = new RealMatrix((m,n) => Real(a(m)(n)),a.length,a(0).length) 
  def asRow(r : Seq[Real]) = new RealMatrix( (m,n) => r(n), 1, r.length)
  def asColumn(r : Seq[Real]) = new RealMatrix( (m,n) => r(m), r.length, 1)
  def construct(f : (Int,Int) => Real, M : Int, N : Int) = new RealMatrix(f,M,N)
  /// Construct RealMatrix from a jblas DoubleMatrix
  //def constructFromJblas(M : org.jblas.DoubleMatrix) = construct( (m,n) => Real(M.get(m,n)), M.rows, M.columns )
  def constructRow(f : (Int) => Real, N : Int) = construct( (m,n) => f(n), 1, N)
  def constructColumn(f : (Int) => Real, M : Int) = construct( (m,n) => f(m), M, 1)
}

/** Matrix with real elements */
class RealMatrix(f : (Int,Int) => Real, override val M : Int, override val N : Int) 
extends MatrixWithElementsFromAField[Real, RealMatrix] {
  
  override protected def get(m : Int, n : Int) = f(m,n)
  override def construct(f : (Int,Int) => Real, M : Int, N : Int) = RealMatrix.construct(f,M,N)
  
  /** Sum of the squared magnitudes of all of the elements */
  lazy val squaredFrobeniusNorm : Double = indices.foldLeft(0.0)( (v, i) => v + this(i).d*this(i).d)
  /** The Frobenius norm, square root of sum of elements squared */
  lazy val frobeniusNorm : Double = sqrt(squaredFrobeniusNorm)
  
  /**
   * Returns (U, S, V) where U is MxM and V is NxN, both orthonormal and S the diagonal matrix such that
   *  this = U S V'
   * where V' is the transpose of V.  Uses the JBlas library, which is very fast but requires some
   * native code. Ultimately, it would be better to have a pure java or scala version of this.
   */
  def singularValueDecomposition : (RealMatrix, RealMatrix, RealMatrix) = {
    throw new UnsupportedOperationException("Not supported")
//    val USV = org.jblas.Singular.sparseSVD(tojblas)
//    val U = RealMatrix.constructFromJblas(USV(0))
//    val S = construct( (m,n) => if(m==n) Real(USV(1).get(m,0)) else Real.zero, USV(1).rows, USV(1).rows )
//    val V = RealMatrix.constructFromJblas(USV(2))
//    return (U, S , V)
  }
  def svd = singularValueDecomposition
  override def smithNormalForm = svd
  
  /**
   * Returns the Moore-Penrose psuedo inverse of this matrix
   */
  def psuedoinverse = {
    if(M < N) this.t*(this*this.t).inv 
    else if(M > N) (this.t*this).inv*this.t
    else inverse
  }
  def pinv = psuedoinverse
  
  /** 
   *QR decomposition based on Householder reflections.  Probably not the most efficient
   *implementation and it's not tail recursive, so it will stack overflow for large matrices, but it looks nice!
   */
  def qr : (RealMatrix, RealMatrix) = {
    val x = column(0)
    val u = x - identity(M,1)*x.frobeniusNorm
    val v = if(u.frobeniusNorm==0) zeros(M,1) else u/u.frobeniusNorm   //handles when first column is (1,0,0,...0)
    def householder(m : RealMatrix) : RealMatrix = m - v*(v.transpose*m)*2.0 //householder reflection about v (this with allocate memory!)
    if(M==1) return (identity(1), this)
    if(N==1) return (householder(identity(M)), householder(this))
    val r1 = householder(this) 
    val (q2, r2) = r1.submatrix(1 until M, 1 until N).qr //now apply qr to submatrix
    val R = construct( (m,n) => if(m==0 || n==0) r1(m,n) else r2(m-1,n-1), M, N) //this is R
    val Q = householder( construct( (m,n) => if(m==0 || n==0) identity(M)(m,n) else q2(m-1,n-1), M, N) )
    return (Q,R)
  }
  override def hermiteNormalForm = qr
  
//  /// Return this matrix as a jblas DoubleMatrix
//  def tojblas = {
//    val A = org.jblas.DoubleMatrix.zeros(M,N)
//    for( m <- 0 until M; n <- 0 until N ) A.put(m,n,this(m,n).d)
//    A
//  }

}