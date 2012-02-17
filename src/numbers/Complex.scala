/* Copyright (c) 2012 the authors listed at the following URL, and/or
the authors of referenced articles or incorporated external code:
http://en.literateprograms.org/Complex_numbers_(Scala)?action=history&offset=20100212205416

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Modified to include operations on Doubles 16/2/2012 Robby McKilliam

Retrieved from: http://en.literateprograms.org/Complex_numbers_(Scala)?oldid=16655
*/

package numbers

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

}

/** Create a complex number by specifying real and imaginary part */
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

