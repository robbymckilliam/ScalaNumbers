/*
 * @author Robby McKilliam
 */

package numbers.finite.optimisation

import scala.annotation.tailrec
import numbers.finite.RealMatrix
import numbers.finite.Real

object MultiVariableOptimisation {
  
  /** 
   * Standard gradient descent to find the minimum of a multivariate function.  You must
   * provide the gradient of the function as an argument.
   * 
   * @param xstart      starting point for gradient decent (a guess at the minimiser)
   * @param df           the gradient of the function we are minimising
   * @param gamma   step control for decent, larger is fast, small is more stable (default = 0.1)
   * @param tol          desired accuracy (default 1e-6)
   * @param ITRMAX    maximum number of iterations (default 1000)
   */
  class GradientDescent(val xstart : RealMatrix, val df : RealMatrix => RealMatrix, val gamma : Double = 0.1, val tol : Double = 1e-6, val ITRMAX : Int = 1000) {
    
    /** Return (f(x), x) where f(x) is the minimum */
    lazy val xmin = run(xstart, xstart + 2*tol, ITRMAX)
    
    @tailrec protected final def run(x : RealMatrix, xprev : RealMatrix, itrnum : Int) : RealMatrix = {
      if(itrnum == 0) {
        println("Warning: Gradient decent reached the maximum number of iterations " + ITRMAX)
        return x
      }
      if((x - xprev).frobeniusNorm < tol) return x
      val xnext = (x - df(x)*gamma).backwitharray
      return run(xnext,x,itrnum-1)
    }
    
  }

}
