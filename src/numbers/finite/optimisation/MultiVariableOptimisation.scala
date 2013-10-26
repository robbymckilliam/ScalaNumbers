/*
 * @author Robby McKilliam
 */

package numbers.finite.optimisation

import numbers.finite.RealMatrix
import numbers.finite.Real
import numbers.ConvergentIteration

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
      throw new ArrayIndexOutOfBoundsException("xstart needs to be a row or column matrix of length, say L. The gradient vector df needs to have the same dimensions as xstart.")
    
    /** The convergent iterator used to run the decesent, you can obtain MaximumIterations exception through this */
    val convergentiteration = new ConvergentIteration[RealMatrix](xstart, x=>(x - df(x)*gamma).backwitharray, (x,y)=>(x-y).frobeniusNorm<tol, ITRMAX)
    /** The vector x that minimises your function */
    lazy val xmin = convergentiteration.limit
    
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
    
    /** The convergent iterator used to run the decesent, you can obtain MaximumIterations exception through this */
    val convergentiteration = {
      if(xstart.isRow) new ConvergentIteration[RealMatrix](xstart, x=>(x - df(x)*H(x).inv).backwitharray, (x,y)=>(x-y).frobeniusNorm<tol, ITRMAX)
      else new ConvergentIteration[RealMatrix](xstart, x=>(x - H(x).inv*df(x)).backwitharray, (x,y)=>(x-y).frobeniusNorm<tol, ITRMAX)
    }
    /** The vector x that minimises your function */
    lazy val xmin = convergentiteration.limit
    
  }
  
}
