/*
 * Contains highly abstracted algorithms
 * @author Robby McKilliam
 */

package numbers

import scala.annotation.tailrec

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
        throw MaximumIterationsReachedException("Warning : ConvergenIteration reached the maximum number " + ITRMAX + " iterations.", x)
      val nextx = step(x)
      if( stop(x,nextx) ) return nextx
      else return run(nextx,itrnum-1)
    }
    
    case class MaximumIterationsReachedException(val message: String = null, val limit : T, val cause: Throwable = null) extends Exception(message, cause)
  
  }

