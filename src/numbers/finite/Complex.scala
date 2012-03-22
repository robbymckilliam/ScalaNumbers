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

  def +(that: Complex) = new RectComplex(real + that.real, imag + that.imag)
  def -(that: Complex) = new RectComplex(real - that.real, imag - that.imag)
  def +(that: Double) = new RectComplex(real + that, imag)
  def -(that: Double) = new RectComplex(real - that, imag)

  def *(that: Complex) = new PolarComplex(magnitude * that.magnitude, angle + that.angle)
  def /(that: Complex) = new PolarComplex(magnitude / that.magnitude, angle - that.angle)
  def *(that: Double) = new PolarComplex(magnitude * that, angle)
  def /(that: Double) = new PolarComplex(magnitude / that, angle)

  def conjugate = new RectComplex(real, -imag)
    
  def one : Complex = Complex.one
  def zero : Complex = Complex.zero
    
  def norm : Real = new Real(mag2)
    
  override def toString : String  = real.toString + " + " + imag.toString + "i"

}

/** Create a complex number by specifying real and imaginary parts */
class RectComplex(r: Double, i: Double) extends Complex {
  val real = r
  val imag = i
  val mag2 = r*r + i*i
  val magnitude = scala.math.sqrt(r*r + i*i)
  val angle = scala.math.atan2(i, r)
}

/** Create a complex number by specifying magnitude and angle (in radians)*/
class PolarComplex(m: Double, a: Double) extends Complex {
  val real = m * scala.math.cos(a)
  val imag = m * scala.math.sin(a)
  val magnitude = m
  val mag2 = m*m
  val angle = a
}

