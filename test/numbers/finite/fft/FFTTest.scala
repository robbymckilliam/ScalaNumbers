/* 

 */

package numbers.finite.fft

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

  
  

}

