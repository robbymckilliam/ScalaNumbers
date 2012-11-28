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

  final def - = new RectComplex(-real, -imag)
  final def +(that: Complex) = new RectComplex(real + that.real, imag + that.imag)
  final def -(that: Complex) = new RectComplex(real - that.real, imag - that.imag)
  final def +(that: Double) = new RectComplex(real + that, imag)
  final def -(that: Double) = new RectComplex(real - that, imag)

  final def *(that: Complex) = new RectComplex(real * that.real - imag*that.imag, 
                                                              real*that.imag+ imag*that.real)
  final def /(that: Complex) = new PolarComplex(magnitude / that.magnitude, angle - that.angle)
  final def *(that: Double) = new RectComplex(real * that, imag * that)
  final def /(that: Double) = new RectComplex(real / that, imag / that)
  final def / = new PolarComplex(1.0 / magnitude, -angle)

  final def pow(e : Double) = new PolarComplex(scala.math.pow(magnitude,e),angle*e)
  
  def conjugate :Complex = new RectComplex(real, -imag)
    
  final def one : Complex = Complex.one
  final def zero : Complex = Complex.zero
    
  def norm : Real = new Real(mag2)
  
  final def ==(that : Complex) = this.real == that.real && this.imag == that.imag
  final def !=(that : Complex) = !(this == that)
  
  /// Don't factorise the complex numbers!
  final def factors = null
    
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

/** Matrix with complex elements */
class ComplexMatrix(f : (Int,Int) => Complex, override val M : Int, override val N : Int) 
  extends MatrixWithElementsFromAField[Complex, ComplexMatrix] {
    
  override def apply(mn : (Int,Int)) = f(mn._1,mn._2)
  override def construct(f : (Int,Int) => Complex, M : Int, N : Int) = new ComplexMatrix(f,M,N)
  
  def singularValueDecomposition = throw new UnsupportedOperationException("not implemented yet")
  def qr = throw new UnsupportedOperationException("not implemented yet")
  override def smithNormalForm = singularValueDecomposition
  override def hermiteNormalForm = qr

}

