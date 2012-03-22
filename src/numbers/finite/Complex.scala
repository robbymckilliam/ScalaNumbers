/* 
 Complex.scala

 Implementation of the Field of complex numbers

 Robby McKilliam 16/2/2012 

 Based on: http://en.literateprograms.org/Complex_numbers_(Scala)?oldid=16655
 */

package numbers.finite

import numbers.Field

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
  val mag2: Double //use norm

  final def +(that: Complex) = new RectComplex(real + that.real, imag + that.imag)
  final def -(that: Complex) = new RectComplex(real - that.real, imag - that.imag)
  final def +(that: Double) = new RectComplex(real + that, imag)
  final def -(that: Double) = new RectComplex(real - that, imag)

  final def *(that: Complex) = new RectComplex(real * that.real - imag*that.imag, 
                                                              real*that.imag+ imag*that.real)
  final def /(that: Complex) = new PolarComplex(magnitude / that.magnitude, angle - that.angle)
  final def *(that: Double) = new RectComplex(real * that, imag * that)
  final def /(that: Double) = new RectComplex(real / that, imag / that)

  def conjugate :Complex = new RectComplex(real, -imag)
    
  final def one : Complex = Complex.one
  final def zero : Complex = Complex.zero
    
  def norm : Real = new Real(mag2)
    
  override def toString : String  = real.toString + " + " + imag.toString + "i"

}

/** Create a complex number by specifying real and imaginary parts */
class RectComplex(r: Double, i: Double) extends Complex {
  val real = r
  val imag = i
  lazy val mag2 = r*r + i*i
  lazy val magnitude = scala.math.sqrt(r*r + i*i)
  lazy val angle = scala.math.atan2(i, r)
  
}

/** Create a complex number by specifying magnitude and angle (in radians)*/
class PolarComplex(m: Double, a: Double) extends Complex {
  val real = m * scala.math.cos(a)
  val imag = m * scala.math.sin(a)
  val magnitude = m
  val mag2 = m*m
  val angle = a  
}

