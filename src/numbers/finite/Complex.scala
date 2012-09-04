/* 
 Complex.scala

 Implementation of the Field of complex numbers with finite double precision.   
 Based on: http://en.literateprograms.org/Complex_numbers_(Scala)?oldid=16655
 
 @author Robby McKilliam

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

abstract class Complex extends Field[Complex, Double]{
  val real: Double
  val imag: Double
  val magnitude: Double
  val angle: Double
  val mag2: Double //use norm

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
    
  def norm : Double = mag2
  
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

