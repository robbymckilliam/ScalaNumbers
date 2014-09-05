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

 Modified to work with JUnit 16/2/2012 Robby McKilliam

 Retrieved from: http://en.literateprograms.org/Complex_numbers_(Scala)?oldid=16655
 */

package numbers.finite

import org.junit.Test;
import org.junit.Assert._;

class ComplexTest {

  val tolerance = 1e-6
  
	
  @Test
  def RectConstructorTest() {
    val c = new RectComplex(1, 1)
    assertEquals(1, c.real, tolerance)
    assertEquals(1, c.imag, tolerance)
    assertEquals(scala.math.sqrt(2.0), c.magnitude, tolerance)
    assertEquals(scala.math.Pi / 4, c.angle, tolerance)
  }

  @Test
  def PolarConstructorTest() {
    val c = new PolarComplex(1, scala.math.Pi / 4)
    assertEquals(scala.math.sqrt(2.0) / 2, c.real, tolerance)
    assertEquals(scala.math.sqrt(2.0) / 2, c.imag, tolerance)
    assertEquals(1, c.magnitude, tolerance)
    assertEquals(scala.math.Pi / 4, c.angle, tolerance)
  }

  @Test
  def AdditionTest() {
    val z1 = new RectComplex(1, 2)
    val z2 = new RectComplex(3, 4)
    val z3 = z1 + z2
    assertEquals(1 + 3, z3.real, tolerance)
    assertEquals(2 + 4, z3.imag, tolerance)
  }
  
  @Test
  def DoubleAdditionTest() {
    val z1 = new RectComplex(1, 2)
    val z2 = 3
    val z3 = z1 + z2
    assertEquals(1 + 3, z3.real, tolerance)
    assertEquals(2.0, z3.imag, tolerance)
  }

  @Test
  def SubractionTest() {
    val z1 = new RectComplex(1, 2)
    val z2 = new RectComplex(3, 4)
    val z3 = z1 - z2
    assertEquals(1 - 3, z3.real, tolerance)
    assertEquals(2 - 4, z3.imag, tolerance)
  }

  @Test
  def MultiplicationTest() {
    val z1 = new PolarComplex(1, scala.math.Pi / 3)
    val z2 = new PolarComplex(3, scala.math.Pi / 6)
    val z3 = z1 * z2
    assertEquals(1 * 3, z3.magnitude, tolerance)
    assertEquals(scala.math.Pi / 2, z3.angle, tolerance)
  }
  
  @Test
  def DoubleMultiplicationTest() {
    val z1 = new PolarComplex(1, scala.math.Pi / 3)
    val z2 = 3.0
    val z3 = z1 * z2
    assertEquals(1 * 3, z3.magnitude, tolerance)
    assertEquals(scala.math.Pi / 3, z3.angle, tolerance)
  }


  @Test
  def DivisionTest() {
    val z1 = new PolarComplex(1, scala.math.Pi / 3)
    val z2 = new PolarComplex(2, scala.math.Pi / 6)
    val z3 = z1 / z2
    assertEquals(1.0 / 2, z3.magnitude, tolerance)
    assertEquals(scala.math.Pi / 6, z3.angle, tolerance)
  }

  @Test
  def ConjugateTest() {
    val z = new RectComplex(1, 2).conjugate
    assertEquals(1, z.real, tolerance)
    assertEquals(-2, z.imag, tolerance)
  }
  
  @Test
  def ComplexImplicits() {
    val tol = 1e-7
    def assertEq(a : Complex, b : Complex) { assertTrue((a-b).norm < tol) }
    assertEq(RectComplex(1,0) + 1,RectComplex(2,0))
    assertEq( 1 + Complex.zero, Complex.one )
    assertEq( 1.0 + Complex.zero, Complex.one )
    assertEq( 1.0 + 2*Complex.zero, Complex.one)
    assertEq( 1.0 + 2*Complex.one, RectComplex(3,0))
    assertEq( 1.0 + 2*RectComplex(1,2), RectComplex(3,4))
  }
  

}

