/*
 * Contains highly abstracted algorithms
 * @author Robby McKilliam
 */

package numbers

import scala.annotation.tailrec

object Algorithms {
  
case class MaximumIterationsReachedException[T](val message: String = null, val limit : T, val cause: Throwable = null) extends Exception(message, cause)

/** 
 * Abstracts the process of iterating until convergence.  The sequence is assumed "Markovian"
 * in the sense that there is a function step that computes the next element from the previous
 * one.
 * 
 * This class is used by numerous iterative algorithms such as gradient asscent, Gauss-Newton etc.
 * 
 * @param start       starting point for iteration
 * @param step        function that returns next value of iteration given previous one
 * @param stop        stopping criterion function.  Take consequetive elements in the sequence and returns true if we should stop iterating.
 * @param ITRMAX    maximum number of iterations (default 1000)
 */
class ConvergentIteration[T](val start: T, val step : T => T, val stop : (T,T) => Boolean, val ITRMAX : Int = 1000) {
  
  /** The limit the algorithm coverged to */
  lazy val limit = run(start, ITRMAX)
  
  @tailrec protected final def run(x: T, itrnum : Int) : T = {
    if(itrnum == 0)
      throw MaximumIterationsReachedException[T]("Warning : ConvergenIteration reached the maximum number " + ITRMAX + " iterations.", x)
    val nextx = step(x)
    if( stop(x,nextx) ) return nextx
    else return run(nextx,itrnum-1)
  }

}

  /** 
    * Evaluates finite simple continued fraction corresponding with sequence a.  
    * The form of the continued fraction is:
    * 
    * a(0) + 1/(a(1) + 1/(a(2) + 1/(a(3) .... )))
    * 
    * @param a          Sequence of integers for this continued fraction
    * @param itof       Function for mapping two "integers" to it's equivalent "rational"
    */
  class ContinuedFraction[F <: Field[F,_]](val a : Seq[F]) {
    if(a.isEmpty) throw new ArrayIndexOutOfBoundsException("a must contain atleast one element for finite continued fraction.")
    //the value of the continued fraction obtained
    lazy val value = run(a.last,a.size-2)
    @tailrec protected final def run(v : F, n : Int) : F = if(n < 0) return v else run(a(n) + v.reciprocal,n-1)
  }
  
  /** 
    * Evaluates generalised finite continued fraction corresponding with sequence a and b.  These
    * sequences nolonger need to contain integers.  The form of the continued fraction is:
    * 
    * b(0) + a(1)/(b(1) + a(2)/(b(2) + a(3)/(b(3) .... a(N)/b(N)))) ... )))
    * 
    * @param a          first sequence for this continued fraction starting at 1
    * @param b          second sequence for this continued fraction starting at 0
    * @param N          number of iterations to compute, i.e., the length.
    */
  class GeneralisedContinuedFraction[F <: Field[F,_]](val a : Int => F, val b : Int => F, val N: Int) {
    //the value of the continued fraction obtained
    lazy val value = run(b(N),N)
    @tailrec protected final def run(v : F, n : Int) : F = if(n == 0) return v else run(b(n-1) + a(n)*v.reciprocal,n-1)
  }
  
   /** 
    * Evaluates the infinite simple continued fraction represented by sequence a.
    * Computed by recursion until tolerance is met or until maximum number of iterations is reached.
    * The form of the continued fraction is:
    * 
    * a(0) + 1/(a(1) + 1/(a(2) + 1/(a(3) .... )))
    * 
    * @param a          Sequence of integers for this continued fraction
    * @param tol        Iteration will stop once output is within tol of intput.  This is guaranteed with infinite precision classes such as Rational
    *                          and will only not occur with finite precision numbers due to rounding error (unlikely).  tol = 0 will run until sequence ends or converges below numerical precision.
    * @param itof       Function for mapping two "integers" to it's equivalent "rational"
    * @param ITRMAX Exception thrown when maximum number of iterations reached. Default = 1000
   */
   class InfiniteContinuedFraction[F <: Field[F,_]](val a : Int => F, val tol : F, val ITRMAX : Int = 1000) {
      lazy val value = new InfiniteGeneralisedContinuedFraction[F](n=>a(0).one,a,tol,ITRMAX).value
    }
    
    /** 
    * Evaluates generalised finite continued fraction corresponding with sequence a and b.  These
    * sequences nolonger need to contain integers.  The form of the continued fraction is:
    * 
    * b(0) + a(1)/(b(1) + a(2)/(b(2) + a(3)/(b(3) .... )))
    * 
    * @param a          first sequence for this continued fraction starting at 1
    * @param b          second sequence for this continued fraction starting at 0
    * @param N          number of iterations to compute, i.e., the length.
    */
    class InfiniteGeneralisedContinuedFraction[F <: Field[F,_]](val a : Int => F, val b : Int => F, val tol: F, val ITRMAX : Int = 1000) {
      val one = b(0).one //the number one for this EuclideanDomain
      val zero = b(0).zero //the number zero for this EuclideanDomain
      
      //the value of the continued fraction obtained
      lazy val value = run(b(0),one,one,zero,1)
    
      @tailrec protected final def run(hn1 : F, hn2 : F, kn1 : F, kn2 : F, n : Int) : F = {
        val hn = b(n)*hn1 + a(n)*hn2
        val kn = b(n)*kn1 + a(n)*kn2
        if(n==ITRMAX) throw MaximumIterationsReachedException[F]("Warning : continued fraction reached the maximum number " + ITRMAX + " iterations.", hn/kn)
        if( kn1 != zero && kn2 != zero )
          if(tol.normlarger(hn1/kn1 - hn2/kn2)) return hn/kn
        return run(hn,hn1,kn,kn1,n+1)
      }
    }
  
}

