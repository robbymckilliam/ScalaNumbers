/* 
Complex.scala

Implementation of the Field of complex numbers

Robby McKilliam 16/2/2012 

Based on: http://en.literateprograms.org/Complex_numbers_(Scala)?oldid=16655
*/

package numbers

/**
 * Provides static definitions of the multiplicative 
 * and additive identities
 */
object Complex {
  val one = new RectComplex(1,0) 
  val zero = new RectComplex(0,0)
}

abstract class Complex extends Field[Complex]{
    val real:      Double
    val imag:      Double
    val magnitude: Double
    val angle:     Double  

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
    
    override def toString : String  = real.toString + " + " + imag.toString + "i"

}

/** Create a complex number by specifying real and imaginary parts */
class RectComplex(r: Double, i: Double) extends Complex {
  val real = r
  val imag = i
  val magnitude = Math.sqrt(r*r + i*i)
  val angle = Math.atan2(i, r)
}

/** Create a complex number by specifying magnitude and angle (in radians)*/
class PolarComplex(m: Double, a: Double) extends Complex {
  val real = m * Math.cos(a)
  val imag = m * Math.sin(a)
  val magnitude = m
  val angle = a
}

