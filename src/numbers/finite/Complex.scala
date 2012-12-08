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

/** Create a complex number by specifying real and imaginary parts */
class RectComplex(val real: Double, val imag: Double) extends Complex {
  lazy val mag2 = real*real + imag*imag
  lazy val magnitude = scala.math.sqrt(real*real + imag*imag)
  lazy val angle = scala.math.atan2(imag, real)
  
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
  def apply(a : Seq[Seq[Complex]]) : ComplexMatrix = new ComplexMatrix((m,n) => a(m)(n),a.length,a(0).length) 
}

/** Matrix with complex elements */
class ComplexMatrix(f : (Int,Int) => Complex, override val M : Int, override val N : Int) 
  extends MatrixWithElementsFromAField[Complex, ComplexMatrix] {
    
  override protected def get(m : Int, n : Int) = f(m,n)
  override def construct(f : (Int,Int) => Complex, M : Int, N : Int) = new ComplexMatrix(f,M,N)
  
  def /(d: Double) = construct( (m,n) => this(m,n)/d, M, N )
  def *(d: Double) = construct( (m,n) => this(m,n)*d, M, N )
  
  /** Hermitian (conjugate) transpose of this matrix */
  def conjugateTranspose = construct( (m,n) => this(n,m).conjugate, N, M)
  final def hermitianTranspose = conjugateTranspose
  
  /** Sum of the squared magnitudes of all of the elements */
  def frobeniusNorm : Double = sqrt( indices.foldLeft(0.0)( (v, i) => v + this(i).mag2) )
  
  def singularValueDecomposition : (ComplexMatrix, ComplexMatrix, ComplexMatrix) = {
    val A = new org.jblas.ComplexDoubleMatrix(M,N)
    for( m <- 0 until M; n <- 0 until N ) A.put(m,n,new org.jblas.ComplexDouble(this(m,n).real, this(m,n).imag) )
    val USV = org.jblas.Singular.sparseSVD(A)
    val U = construct( (m,n) => new RectComplex(USV(0).get(m,n).real, USV(0).get(m,n).imag), USV(0).rows, USV(0).columns )
    val S = construct( (m,n) => if(m==n) new RectComplex(USV(1).get(m,0).real, USV(1).get(m,0).imag) else Complex.zero, USV(1).rows, USV(1).rows )
    val V = construct( (m,n) => new RectComplex(USV(2).get(m,n).real, -USV(2).get(m,n).imag), USV(2).rows, USV(2).columns ) //JBlas output is wrong here, conjugate required!
    return (U,S,V)
  }
  def svd = singularValueDecomposition
  override def smithNormalForm = svd
  
  /** 
   * Returns the pseudoinverse of this complex matrix.  Uses the singular value decomposition.
   */
  def inverse = {
    val (u,s,v) = this.svd
    val sinv = construct( (m,n) => if(m==n) Complex.one/s(m,n) else Complex.zero, s.M, s.N)
    v*sinv*u.conjugateTranspose
  }
  
  def qr = throw new UnsupportedOperationException("not implemented yet")
  override def hermiteNormalForm = qr
  def lu = throw new UnsupportedOperationException("not implemented yet")
}

