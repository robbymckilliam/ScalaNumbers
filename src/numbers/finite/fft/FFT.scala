package numbers.finite.fft

import numbers.finite.Complex
import numbers.finite.RectComplex

trait DiscreteFourierTransform {
  def forward(x : Seq[Complex]) : Seq[Complex]
  def inverse(x : Seq[Complex]) : Seq[Complex]
}

object DiscreteFourierTransform {
  
   /** Compute a convolution using the fft.  The output is identical to Matlab's conv command */
  def conv(a : Seq[Complex], b : Seq[Complex]) : Seq[Complex] = {
    val L = a.size + b.size - 1
    val fft = new JTransformsFFT(L)
    val afft = fft.forward(a)
    val bfft = fft.forward(b)
    val cfft = (0 until L).map(i => afft(i) * bfft(i))
    return fft.inverse(cfft)
  }
  
  /** 
   * Compute a convolution using the fft with the convolution `tails' removed.  
   * The output is identical to Matlab's:
   * conv(a,b,`valid')  when the length(a) >= length(b),  and 
   * conv(b,a,`valid') when length(a) =< length(b).
   */
  def conv_valid(a : Seq[Complex], b : Seq[Complex]) : Seq[Complex] = {
    val L = a.size + b.size - 1
    val M = scala.math.min(a.size,b.size)
    val c = conv(a,b)
    return c.slice(M-1,L-M+1)
  }
  
  /** Compute the discrete Fouier transform of x.  Uses the JTransforms library. */
  def fft(x: Seq[Complex]) : Seq[Complex] = new JTransformsFFT(x.size).forward(x)
  
  /** Compute the inverse discrete Fourier transform of x. Uses the JTransforms library. */
  def ifft(x: Seq[Complex]) : Seq[Complex] = new JTransformsFFT(x.size).inverse(x)

}

/** Wraps the JTransforms FFT library */
class JTransformsFFT(N : Int) {
  private val jtFFT = new edu.emory.mathcs.jtransforms.fft.DoubleFFT_1D(N)
  edu.emory.mathcs.utils.ConcurrencyUtils.setNumberOfThreads(1) //just use one thread for FFTs

  private val mem = new Array[Double](2*N) 
  
  /** 
   *Compute the discrete Fourier transform of x.  If x.size is smaller than N then the
   *zero padded Fourier transform is computed
   */
  def forward(x : Seq[Complex]) : Seq[Complex] = {
    if(x.size > N) throw new RuntimeException("x is larger than the FFT size")
    tomem(x)
    jtFFT.complexForward(mem)
    return frommem
  }
  
  def inverse(x : Seq[Complex]) : Seq[Complex] = {    
    if(x.size != N) throw new RuntimeException("x is the wrong size")
    tomem(x)
    jtFFT.complexInverse(mem, true)
    return frommem
  }
  
  //fill internal memory with x, zero pad if required
  private final def tomem(x : Seq[Complex]) = {
    for(i <- 0 until x.size) { //would probably need a while loop to make this fast?
      mem(2*i) = x(i).real
      mem(2*i+1) = x(i).imag
    }
    for(i <- x.size until N){
      mem(2*i) = 0
      mem(2*i+1) = 0
    }
  }
  
  //get a complex sequence from the internal memory
  private final def frommem : Seq[Complex] = {
    (0 until N).map( i => new RectComplex(mem(2*i), mem(2*i+1)) )
  }
  
}