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
   * @param df           the gradient of the function to be minimised
   * @param gamma   step control for decent, larger is fast, small is more stable (default = 0.1)
   * @param tol          desired accuracy (default 1e-6)
   * @param ITRMAX    maximum number of iterations (default 1000)
   */
  class GradientDescent(val xstart : RealMatrix, val df : RealMatrix => RealMatrix, val gamma : Double = 0.1, val tol : Double = 1e-6, val ITRMAX : Int = 1000) {
    if( !xstart.isRow && !xstart.isColumn ) 
      throw new ArrayIndexOutOfBoundsException("xstart needs to be a row or column matrix of length, say L. The gradient vector df need to have the same dimensions as xstart.")
    
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
  
  /** 
   * The Newton-Raphson proceedure for finding a stationary point of a multivariate function. 
   * It could be either a minimum or a maximum or even a saddle point.  
   * Convergence is fast (quadratic).  Requires gradient vector and Hessian matrix.  
   * 
   * @param xstart     starting point (a guess at the minimiser/maximiser)
   * @param df           the gradient of the function to be minimised
   * @param H            the Hessian matrix of the function to be minimised
   * @param tol          desired accuracy (default 1e-6)
   * @param ITRMAX    maximum number of iterations (default 100)
   */
  class NewtonRaphson(val xstart : RealMatrix, val df : RealMatrix => RealMatrix, val H : RealMatrix => RealMatrix, val tol : Double = 1e-6, val ITRMAX : Int = 100) {
    if( !xstart.isRow && !xstart.isColumn ) 
      throw new ArrayIndexOutOfBoundsException("xstart needs to be a row or column matrix of length, say L. The gradient vector df need to have the same dimensions as xstart and the Hessian H needs to be and L by L matrix")
    
    /** Return (f(x), x) where f(x) is the minimum */
    lazy val xmin = run(xstart, xstart + 2*tol, ITRMAX)
    
    @tailrec protected final def run(x : RealMatrix, xprev : RealMatrix, itrnum : Int) : RealMatrix = {
      if(itrnum == 0) {
        println("Warning: Newton-Raphson method reached the maximum number of iterations " + ITRMAX)
        return x
      }
      if((x - xprev).frobeniusNorm < tol) return x
      val xnext = {
        if(x.isRow) (x - df(x)*H(x).inv).backwitharray //get order of multiplication the right way regardless of whether input x is column of row vector
        else (x - H(x).inv*df(x)).backwitharray
      }
      return run(xnext,x,itrnum-1)
    }
    
  }

}
