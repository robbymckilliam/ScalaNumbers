/* 
 Complex.scala

 Implementation of the Field of complex numbers with finite double precision and a Matrix
 containing complex numbers
 Complex class based on: http://en.literateprograms.org/Complex_numbers_(Scala)?oldid=16655
 
 @author Robby McKilliam

 */

package numbers.finite

import numbers.Field
import numbers.matrix.MatrixWithElementsFromAField
import scala.math.sqrt

/**
 * Provides static definitions of the multiplicative 
 * and additive identities
 */
object Complex {
  val one = new RectComplex(1,0) 
  val zero = new RectComplex(0,0)
}

abstract class Complex extends Field[Complex, Real]{
  val real: Double
  val imag: Double
  val magnitude: Double
  val angle: Double
  val mag2: Double

  final override def unary_- = new RectComplex(-real, -imag)
  final override def +(that: Complex) = new RectComplex(real + that.real, imag + that.imag)
  final override def -(that: Complex) = new RectComplex(real - that.real, imag - that.imag)
  final def +(that: Double) = new RectComplex(real + that, imag )
  final def -(that: Double) = new RectComplex(real - that, imag )
  final override def *(that: Complex) = new RectComplex(real * that.real - imag*that.imag, 
                                                              real*that.imag+ imag*that.real)
  final override def /(that: Complex) = new PolarComplex(magnitude / that.magnitude, angle - that.angle)
  final def *(that: Double) = new RectComplex(real * that, imag * that)
  final def /(that: Double) = new RectComplex(real / that, imag / that)

  final def pow(e : Double) = new PolarComplex(scala.math.pow(magnitude,e),angle*e)
  
  def conjugate :Complex = new RectComplex(real, -imag)
    
  final override def one : Complex = Complex.one
  final override def zero : Complex = Complex.zero
    
  def norm : Real = new Real(mag2)
  
  final override def ==(that : Complex) = this.real == that.real && this.imag == that.imag
    
  override def toString : String  = real.toString + " + " + imag.toString + "i"

}

object RectComplex {
  def apply(r : Double, i : Double) = new RectComplex(r,i)
}

/** Create a complex number by specifying real and imaginary parts */
class RectComplex(val real: Double, val imag: Double) extends Complex {
  lazy val mag2 = real*real + imag*imag
  lazy val magnitude = scala.math.sqrt(real*real + imag*imag)
  lazy val angle = scala.math.atan2(imag, real)
  
}

object PolarComplex {
  def apply(m : Double, a : Double) = new PolarComplex(m,a)
}

/** Create a complex number by specifying magnitude and angle (in radians)*/
class PolarComplex(val magnitude: Double, val angle: Double) extends Complex {
  lazy val real = magnitude * scala.math.cos(angle)
  lazy val imag = magnitude * scala.math.sin(angle)
  lazy val mag2 = magnitude*magnitude 
}

object ComplexMatrix {
  def asRow(r : Seq[Complex]) = new ComplexMatrix( (m,n) => r(n), 1, r.length)
  def asColumn(r : Seq[Complex]) = new ComplexMatrix( (m,n) => r(m), r.length, 1)
  //def asRow(r : Seq[Double]) = new ComplexMatrix( (m,n) => new RectComplex(r(n),0), 1, r.length)
  //def asColumn(r : Seq[Double]) = new ComplexMatrix( (m,n) => new RectComplex(r(n),0), r.length, 1)

  def apply(a : Seq[Seq[Complex]]) : ComplexMatrix = new ComplexMatrix((m,n) => a(m)(n),a.length,a(0).length) 
  def apply(f : (Int,Int) => Complex, M : Int, N : Int) : ComplexMatrix = new ComplexMatrix((m,n) => f(m,n),M,N) 
    /// contruct identity matrix
  def identity(M : Int, N : Int) : ComplexMatrix = new ComplexMatrix( (m,n) => if(m==n) Complex.one else Complex.zero, M,N)
  def identity(N : Int): ComplexMatrix = identity(N,N)
  
  def construct(f : (Int,Int) => Complex, M : Int, N : Int) = new ComplexMatrix(f,M,N)
  /// construct a ComplexMatrix from a jblas ComplexDoubleMatrix
  def constructFromJblas(M : org.jblas.ComplexDoubleMatrix) =construct( (m,n) => new RectComplex(M.get(m,n).real, M.get(m,n).imag), M.rows, M.columns )
  def constructRow(f : (Int) => Complex, N : Int) = construct( (m,n) => f(n), 1, N)
  def constructColumn(f : (Int) => Complex, M : Int) = construct( (m,n) => f(m), M, 1)
}

/** Matrix with complex elements */
class ComplexMatrix(f : (Int,Int) => Complex, override val M : Int, override val N : Int) 
  extends MatrixWithElementsFromAField[Complex, Real, ComplexMatrix] {
    
  override protected def get(m : Int, n : Int) = f(m,n)
  override def construct(f : (Int,Int) => Complex, M : Int, N : Int) = ComplexMatrix.construct(f,M,N)
  
  def /(d: Double) = construct( (m,n) => this(m,n)/d, M, N )
  def *(d: Double) = construct( (m,n) => this(m,n)*d, M, N )
  
  /** Hermitian (conjugate) transpose of this matrix */
  def conjugateTranspose = construct( (m,n) => this(n,m).conjugate, N, M)
  final def hermitianTranspose = conjugateTranspose
  final def h = hermitianTranspose
  
  /** Sum of the squared magnitudes of all of the elements */
  lazy val squaredFrobeniusNorm : Double = indices.foldLeft(0.0)( (v, i) => v + this(i).mag2)
  /** The Frobenius norm, square root of sum of elements squared */
  lazy val frobeniusNorm : Double = sqrt(squaredFrobeniusNorm)
  
  def singularValueDecomposition : (ComplexMatrix, ComplexMatrix, ComplexMatrix) = {
    val USV = org.jblas.Singular.fullSVD(tojblas)
    val U = ComplexMatrix.constructFromJblas(USV(0))
    val S = construct( (m,n) => if(m==n) new RectComplex(USV(1).get(m,0).real, USV(1).get(m,0).imag) else Complex.zero, USV(1).rows, USV(1).rows )
    val V = construct( (m,n) => new RectComplex(USV(2).get(m,n).real, -USV(2).get(m,n).imag).conjugate, USV(2).rows, USV(2).columns ) //god knows why but Jblas returns conjugate again???
    return (U,S,V)
  }
  def svd = singularValueDecomposition
  override def smithNormalForm = svd
  
   /** 
   * QR decomposition based on Householder reflections.  Probably not the most efficient
   * implementation and it's not tail recursive, so it will stack overflow for large matrices, but it looks nice!
   */
  def qr : (ComplexMatrix, ComplexMatrix) = {
    val x = column(0)
    val u = x - identity(M,1) * PolarComplex(x.frobeniusNorm,x(0,0).angle)
    val v = if(u.frobeniusNorm==0) zeros(M,1) else u/u.frobeniusNorm   //handles when first column is (1,0,0,...0)
    val w = if(u.frobeniusNorm==0) Complex.zero else Complex.one +  (x.hermitianTranspose * v)(0,0) / (v.hermitianTranspose * x)(0,0)
    def householder(m : ComplexMatrix) : ComplexMatrix = m - v*(v.hermitianTranspose*m)*w //householder reflection about v
    if(M==1) return (identity(1), this)
    if(N==1) return (householder(identity(M)), householder(this))
    val r1 = householder(this) 
    val (q2, r2) = r1.submatrix(1 until M, 1 until N).qr //now apply qr to submatrix
    val R = construct( (m,n) => if(m==0 || n==0) r1(m,n) else r2(m-1,n-1), M, N) //this is R
    val Q = householder( construct( (m,n) => if(m==0 || n==0) identity(M)(m,n) else q2(m-1,n-1), M, N) )
    return (Q,R)
  }
  override def hermiteNormalForm = qr
  
  /** 
   * Returns the pseudoinverse of this complex matrix.  Uses the singular value decomposition.
   */
  def inverse = {
    if(N != M) throw new ArrayIndexOutOfBoundsException("Matrix is not square, it can't be inverted.  You might want to use psuedoinverse instead.")
    val (u,s,v) = this.svd
    val sinv = construct( (m,n) => if(m==n) Complex.one/s(m,n) else Complex.zero, s.M, s.N)
    v*sinv*u.conjugateTranspose
  }
  def inv = inverse
  
  /**
   * Returns the Moore-Penrose psuedo inverse of this matrix
   */
  def psuedoinverse = {
    if(M < N) this.h*(this*this.h).inverse 
    else if(M > N) (this.h*this).inverse*this.h
    else inverse
  }
  def pinv = psuedoinverse
  
  /** 
   *Determinant of this matrix.  
   *Pretty sure this has a bug in it meaning that the negative of the determinant will sometimes
   *be returned
   */
  lazy val determinant = {
    if(N!=M) throw new ArrayIndexOutOfBoundsException("Only square matrices have determinants!")
    val (u,s,v) = svd
    (0 until N).foldLeft(Complex.one)( (prod, n) => prod*s(n,n) )
  }
  lazy val det = determinant
  
  /// Return this matrix as a jblas ComplexDoubleMatrix
  def tojblas = {
    val A = new org.jblas.ComplexDoubleMatrix(M,N)
    for( m <- 0 until M; n <- 0 until N ) A.put(m,n,new org.jblas.ComplexDouble(this(m,n).real, this(m,n).imag) )
    A
  }
  
}

