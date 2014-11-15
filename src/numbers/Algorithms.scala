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
    * Evaluates finite contiued fraction corresponding with sequence a
    * 
    * @param a          Sequence of integers for this continued fraction
    * @param itof       Function for mapping two "integers" to it's equivalent "rational"
    */
  class FiniteContinuedFraction[F <: Field[F,_], I <: EuclideanDomain[I,_]](val a : Seq[I], val itof : (I,I)=> F) {
    if(a.isEmpty) throw new ArrayIndexOutOfBoundsException("a must contain atleast one element for finite continued fraction.")
    val one = a(0).one //the number one for this EuclideanDomain
    //the value of the continued fraction obtained
    lazy val value = run(itof(a.last,one),a.size-2)
    @tailrec protected final def run(v : F, n : Int) : F = if(n < 0) return v else run(itof(a(n),one) + v.reciprocal,n-1)
  }
  
   /** 
    * Evaluates the infinite simple continued fraction represented by sequence a.
    * Computed by recursion until tolerance is met or until maximum number of iterations is reached
    * 
    * @param a          Sequence of integers for this continued fraction
    * @param tol        Iteration will stop once output is within tol of intput.  This is guaranteed with infinite precision classes such as Rational
    *                          and will only not occur with finite precision numbers due to rounding error (unlikely).  tol = 0 will run until sequence ends or converges below numerical precision.
    * @param itof       Function for mapping two "integers" to it's equivalent "rational"
    * @param ITRMAX Exception thrown when maximum number of iterations reached. Default = 1000
   */
   class InfiniteContinuedFraction[F <: Field[F,_], I <: EuclideanDomain[I,_]](val a : Int => I, val itof : (I,I)=> F,  val tol : F, val ITRMAX : Int = 1000) {
      
      val one = a(0).one //the number one for this EuclideanDomain
      val zero = a(0).zero //the number zero for this EuclideanDomain
      
      //the value of the continued fraction obtained
      lazy val value = run(one,zero,zero,one,0,false)
    
      @tailrec protected final def run(hn1 : I, hn2 : I, kn1 : I, kn2 : I, n : Int, accuracyReached : Boolean) : F = {
        val hn = a(n)*hn1 + hn2
        val kn = a(n)*kn1 + kn2
        if(accuracyReached) return itof(hn,kn)
        if (n == ITRMAX ) throw MaximumIterationsReachedException[F]("Warning : continued fraction reached the maximum number " + ITRMAX + " iterations.", itof(hn,kn))
        val d1 = kn*kn1
        val d2 =kn*(kn1+kn)
        val accurate : Boolean = { //true if the next iteration will have accuracy better than tol
          if( d1 == zero || d2 == zero ) false
          else if(tol.normlarger(itof(one,d1) - itof(one,d2))) true //the partial fraction is now atleast this close
          else false
        }
        return run(hn,hn1,kn,kn1,n+1,accurate)
      }

    }
  
}

