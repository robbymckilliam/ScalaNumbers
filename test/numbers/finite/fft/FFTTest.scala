/* 

 */

package numbers.finite.fft

import numbers.finite.Complex;
import numbers.finite.RectComplex;
import org.junit.Test;
import org.junit.Assert._;

class FFTTest {

  val tolerance = 1e-6
  
	
  @Test
  def versusMatlabTest() {
    val x = Array(new RectComplex(1,0), new RectComplex(2,0), new RectComplex(3,0))
    val fft = new JTransformsFFT(x.size)
    val fftx = fft.forward(x)
    val expected = Array(new RectComplex(6,0), new RectComplex(-1.5,0.866025403784439), new RectComplex(-1.5,-0.866025403784439))
    for( i <- fftx.indices ) {
      assertEquals(expected(i).real, fftx(i).real, tolerance)
      assertEquals(expected(i).imag, fftx(i).imag, tolerance)
    }
    
    val ifftx = fft.inverse(fftx)
    for( i <- x.indices ) {
      assertEquals(x(i).real, ifftx(i).real, tolerance)
      assertEquals(x(i).imag, ifftx(i).imag, tolerance)
    }
    
  }
  
  @Test
  def convTest() {
    val a = Array(new RectComplex(1,0), new RectComplex(2,1))
    val b = Array(new RectComplex(3,0), new RectComplex(2,0), new RectComplex(1,0))
    val c = DiscreteFourierTransform.conv(a,b)
    val expected = Array(new RectComplex(3,0), new RectComplex(8,3), new RectComplex(5,2), new RectComplex(2,1))
    //for( i <- c ) println(i)
    for( i <- c.indices ) {
      assertEquals(expected(i).real, c(i).real, tolerance)
      assertEquals(expected(i).imag, c(i).imag, tolerance)
    }
    val cvalid = DiscreteFourierTransform.conv_valid(a,b)
    val expvalid = Array(new RectComplex(8,3), new RectComplex(5,2))
    //for( i <- cvalid ) println(i)
    for( i <- expvalid.indices ) {
      assertEquals(expvalid(i).real, cvalid(i).real, tolerance)
    }
    
  }

  

}

