/*
 * @Robby McKilliam
 */

package numbers.finite

import numbers.Field
import numbers.matrix.MatrixWithElementsFromAField
import scala.math.min
import scala.math.max
import scala.math.sqrt
/**
 * Provides static definitions of the multiplicative and additive identities.
 */
object Real {
  val one = new Real(1.0) 
  val zero = new Real(0.0)
  
  def apply(x : Double) = new Real(x)
}

/**
 *  This is a potential candidate for scala 2.10's value classes.
 */
class Real(val d : Double) extends Field[Real, Real] with Ordered[Real] {
  //@inline final def toDouble = d
  
  final def - = new Real(-d)
  final def +(that: Real) = new Real(d + that.d)
  final def -(that: Real) = new Real(d - that.d)
  final def +(that: Double) : Real = new Real(d + that)
  final def -(that: Double) : Real = new Real(d - that)
  final def +(that: Int) : Real = new Real(d + that)
  final def -(that: Int) : Real = new Real(d - that)

  final def *(that: Real) = new Real(d * that.d)
  final def /(that: Real) = new Real(d / that.d)
  final def *(that: Double) : Real = new Real(d * that)
  final def /(that: Double) : Real = new Real(d / that)
  final def / : Real = new Real(1.0 / d)
  final def *(that: Int) : Real = new Real(d * that)
  final def /(that: Int) : Real = new Real(d / that)
  
  final def one : Real = Real.one
  final def zero : Real = Real.zero
    
  final def norm : Real = new Real(d.abs)
    
  /// Don't factorise the real numbers!
  final def factors = throw new UnsupportedOperationException("Don't factorise the real numbers!")
  
  final def ==(that : Double) = this.d == that
  final def <=(that : Double) = this.d <= that
  final def <(that : Double) = this.d < that
  final def >=(that : Double) = this.d >= that
  final def >(that : Double) = this.d > that
  final def !=(that : Double) = !(this == that)
  
  /** Uses scala's internal Ordered, only need to override compare */
  final def compare(that : Real) : Int = this.d.compare(that.d)
  
  final override def toString : String  = d.toString
}


object RealMatrix {
  /// contruct identity matrix
  def identity(M : Int, N : Int) : RealMatrix = new RealMatrix( (m,n) => if(m==n) Real.one else Real.zero, M,N)
  def identity(N : Int): RealMatrix = identity(N,N)
  def apply(f : (Int,Int) => Double, M : Int, N : Int) : RealMatrix = new RealMatrix((m,n) => Real(f(m,n)),M,N) 
}

/** Matrix with real elements */
class RealMatrix(f : (Int,Int) => Real, override val M : Int, override val N : Int) 
extends MatrixWithElementsFromAField[Real, RealMatrix] {
    
  override def apply(mn : (Int,Int)) = f(mn._1,mn._2)
  override def construct(f : (Int,Int) => Real, M : Int, N : Int) = new RealMatrix(f,M,N)
  
  /** Sum of the squared magnitudes of all of the elements */
  lazy val frobeniusNorm : Double = sqrt(indices.foldLeft(0.0)( (v, i) => v + this(i).d*this(i).d))
  
  def /(d: Double) : RealMatrix = construct( (m,n) => this(m,n) / d, M, N )
  def *(d: Double) : RealMatrix= construct( (m,n) => this(m,n) * d, M, N )
  
  /** 
   * Returns (U, S, V) where U is MxM and V is NxN, both orthonormal and S the diagonal matrix such that
   *  this = U S V'
   * where V' is the transpose of V.  Uses the JBlas library, which is very fast but requires some
   * native code. Ultimately, it would be better to have a pure java or scala version of this.
   */
  def singularValueDecomposition : (RealMatrix, RealMatrix, RealMatrix) = {
    val A = org.jblas.DoubleMatrix.zeros(M,N)
    for( m <- 0 until M; n <- 0 until N ) A.put(m,n,this(m,n).d)
    val USV = org.jblas.Singular.sparseSVD(A)
    val U = construct( (m,n) => Real(USV(0).get(m,n)), USV(0).rows, USV(0).columns )
    val S = construct( (m,n) => if(m==n) Real(USV(1).get(m,0)) else Real.zero, USV(1).rows, USV(1).rows )
    val V = construct( (m,n) => Real(USV(2).get(m,n)), USV(2).rows, USV(2).columns )
    return (U,S,V)
  }
  def svd = singularValueDecomposition
  override def smithNormalForm = svd
  
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
  
  def lu = throw new UnsupportedOperationException("not implemented yet")
  def inverse = throw new UnsupportedOperationException("not implemented yet")

}