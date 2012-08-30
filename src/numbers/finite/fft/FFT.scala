package numbers.finite.fft

import numbers.finite.Complex
import numbers.finite.RectComplex

trait DiscreteFourierTransform {
  def forward(x : Seq[Complex]) : Seq[Complex]
  def inverse(x : Seq[Complex]) : Seq[Complex]
}

/** Wraps the JTransforms FFT library */
class JTransformsFFT(N : Int) extends DiscreteFourierTransform {
    
  private val jtFFT = new edu.emory.mathcs.jtransforms.fft.DoubleFFT_1D(N)
  private val mem = new Array[Double](2*N) 
  
  def forward(x : Seq[Complex]) : Seq[Complex] = {
    if(x.size != N) throw new RuntimeException("x is the wrong size")
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
  
  private final def tomem(x : Seq[Complex]) = {
    for(i <- 0 until N) {
      mem(2*i) = x(i).real
      mem(2*i+1) = x(i).imag
    }
  }
  
  private final def frommem : Seq[Complex] = {
    val ret = new Array[Complex](N)
    for(i <- 0 until N) ret(i) = new RectComplex(mem(2*i), mem(2*i+1))
    return ret
  }
  
}


