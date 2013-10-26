/*
 * ScalaNumbers.
 * @author Robby McKilliam
 */

package numbers.finite.optimisation

import scala.math.signum
import scala.math.abs
import scala.math.sqrt
import numbers.ConvergentIteration
import scala.math.signum

object SingleVariableOptimisation {
  
  /** 
   *Search for the minimum of the function f(x) in the interval [a, b].
   *Finds a solution such that |x0 - x| < tol where x0 is the minimiser of f
   *Uses Brent's method, a combination of parabolic and golden section search
   *
   *@param f           The function to minimise 
   *@param  a         Left endpoint of initial interval
   *@param  c         Right endpoint of initial interval
   *@param  b         b must satisfy a < b < c and f(a) > f(b) < f(c)
   *@param  tol         Desired length of the interval in which the minimum will be determined to lie (default 1e-6)
   *@params ITRMAX  maximum number of iterations before terminating (default 100)
   */
  def fmin(f : Double => Double, a : Double, b: Double, c : Double, tol : Double = 1e-6, ITRMAX : Int = 100) : (Double, Double) = {
    return new Brent(f,a,b,c,tol, ITRMAX).min
  }
    
  /** 
   *Search for the maximum of the function f(x) in the interval [a, b].
   *Finds a solution such that |x0 - x| < tol where x0 is the maximiser of f.
   *Uses Brent's method, a combination of parabolic and golden section search
   *
   *@param f           The function to maximise 
   *@param  a         Left endpoint of initial interval
   *@param  c         Right endpoint of initial interval
   *@param  b         b must satisfy a < b < c and f(a) > f(b) < f(c)
   *@param  tol         Desired length of the interval in which the minimum will be determined to lie (default 1e-6)
   *@params ITRMAX  maximum number of iterations before terminating (default 100)
   */
  def fmax(f : Double => Double, a : Double, b: Double, c : Double, tol : Double = 1e-6, ITRMAX : Int = 100) : (Double, Double) = {
    val (minf, x) =  fmin(-f(_),a,b,c,tol)
    return (-minf, x)
  }
  
  /** 
   *Search for a zero of the function f(x) in the interval [a, b].
   *Finds a solution such that |x0 - x| < tol where f(x0) = 0.
   *Uses the bisection method, only guaranteed to converge if there is a unique zero 
   *between a and b and f is continuous and sign(f(a)) = - sign(f(b))
   *
   *@param f           The function to zero 
   *@param  a         Left endpoint of initial interval
   *@param  c         Right endpoint of initial interval
   *@param  b         bx must satisfy a < b < c and f(a) > f(b) < f(c)
   *@param  tol         Desired length of the interval in which the minimum will be determined to lie (default 1e-6)
   *@params ITRMAX  maximum number of iterations before terminating (default 100)
   */
  def fzero(f : Double => Double, a : Double, b: Double, tol : Double = 1e-6, ITRMAX : Int = 100) : Double = {
    return new Bisection(f,a,b,tol,ITRMAX).zero
  }

  
  /** 
   *Search for a zero of the function f(x) in the interval [a, b].
   *Finds a solution such that |x0 - x| < tol where f(x0) = 0.
   *Uses the bisection method, only guaranteed to converge if there is a unique zero 
   *between a and b and f is continuous and sign(f(a)) = - sign(f(b))
   *
   *@param  f          The function to zero 
   *@param  a         Left endpoint of initial interval
   *@param  c         Right endpoint of initial interval
   *@param  b         bx must satisfy a < b < c and f(a) > f(b) < f(c)
   *@param  tol         Desired length of the interval in which the minimum will be determined to lie (default 1e-6)
   *@param  ITRMAX  maximum number of iterations before terminating (default 100)
   */
  class Bisection(val f : Double => Double, val ax : Double, val bx : Double, val tol : Double, val ITRMAX : Int) {
  
    /** Return (f(x), x) where f(x) = 0 */
    lazy val zero = run
  
    protected def run : Double = {
      var a = ax; var b = bx
      for( i <- 1 to ITRMAX ){
        var c = (a + b)/2
        var fc = f(c)
        if( fc == 0 || (a-b).abs/2 < tol ) return c
        if( signum(fc) == signum(f(a)) ) a = c else b = c
      }
      throw new RuntimeException("Bisection failed.  Maximum number " + ITRMAX + " of iterations exceeded")
    }

  }

  /**
   *Performs a 1-dimensional minimization.
   *It implements Brent's method which combines a golden-section
   *search and parabolic interpolation.  
   *
   * Ported from Numerical Recipes in C pages 404 - 405
   *
   *@param  f           The function to minimise 
   *@param  ax         Left endpoint of initial interval
   *@param  cx         Right endpoint of initial interval
   *@param  bx         bx must satisfy ax < bx < cx and f(ax) > f(bx) < f(cx)
   *@param  tol         Desired length of the interval in which the minimum will be determined to lie
   *@param  ITRMAX  maximum number of iterations before terminating
   */
  class Brent(val f : Double => Double, val ax : Double, val bx : Double, val cx: Double, val tol : Double, val ITRMAX : Int) {

    protected val ZEPS = 1e-10 //close to machine precision
    protected val C = (3.0 - sqrt(5.0))/2.0;

    /** Return (f(x), x) where f(x) is the minimum */
    lazy val min =  run

    protected def run : (Double, Double) = {
      var e = 0.0
      var d = 0.0
      var a = if(ax < cx) ax else cx
      var b = if(ax > cx) ax else cx
      var x = bx; var w= bx; var v = bx
      var fw = f(x); var fv = fw; var fx = fw
      for( iter <- 1 to ITRMAX ){
        val xm = 0.5*(a+b)
        val tol1 = tol*abs(x)+ZEPS
        val tol2 = 2.0*tol1
        if(abs(x-xm) <= tol2 - 0.5*(b-a)) {
          return (fx, x)
        }
        if( abs(e) > tol1 ){
          val r = (x-w)*(fx-fv)
          var q = (x-v)*(fx-fv)
          var p = (x-v)*q - (x-v)*r
          q = 2.0*(q-r)
          if(q > 0.0) p = -p
          q = abs(q)
          val etemp = e
          e=d
          if( abs(p) >= abs(0.5*q*etemp) || p <= q*(a-x) || p >= q*(b-x) ){ //golden step
            e = if(x >= xm) a-x else b-x
            d = C*e
          }
          else{ //parabolic step
            d=p/q
            val u=x+d
            if(u-a < tol2 || b-u < tol2 ) d = sign(tol1,xm-x)
          }
        }
        else {
          e = if(x >= xm) a-x else b-x
          d = C*e
        }
        val u = if(abs(d) >= tol1) x+d else x + sign(tol1,d)
        val fu=f(u)
        if(fu <= fx){
          if(u >= x) a=x else b=x
          v=w; w=x; x=u
          fv=fw; fw=fx; fx=fu
        }
        else{
          if(u < x) a=u else b=u
          if(fu <= fw || w == x){
            v=w; w=u
            fv=fw; fw=fu
          }
          else if(fu <= fv || v ==x || v == w){
            v=u
            fv=fu
          }
        }
      }
      println("Warning: Brent's method reached the maximum number " + ITRMAX + " iterations")
      return (fx, x)
    }

    @inline protected final def sign(a : Double, b : Double) = abs(a)*signum(b)
  
  }
  
  
  /** 
   * Standard gradient descent to find the minimum of a function.  You must provide the derivative
   * of the function as an argument.  
   * 
   * @param xstart     starting point for gradient decent (a guess at the minimiser)
   * @param df           the derivative of the function being minimising
   * @param gamma   step control for decent, larger is fast, small is more stable (default = 0.1)
   * @param tol          desired accuracy (default 1e-6)
   * @param ITRMAX    maximum number of iterations (default 1000)
   */
  class GradientDescent(val xstart : Double, val df : Double => Double, val gamma : Double = 0.1, val tol : Double = 1e-6, val ITRMAX : Int = 1000) {
    
    /** The convergent iterator used to run the decesent, you can obtain MaximumIterations exception through this */
    val convergentiteration = new ConvergentIteration[Double](xstart, x=>x-gamma*df(x), (x,y)=>(x-y).abs<tol, ITRMAX)
    /** The Double x that minimises your function */
    lazy val xmin = convergentiteration.limit
    
  }
  
  /** 
   * The Newton-Raphson proceedure for finding a stationary point.  It could be either a minimum
   * or a maximum.  Convergence is fast (quadratic).  Requires first and second derivatives.  
   * 
   * @param xstart     starting point (a guess at the minimiser/maximiser)
   * @param df           the derivative of the function being minimised/maximised
   * @param d2f         the second derivative of the function being minimised/maximised
   * @param tol          desired accuracy (default 1e-6)
   * @param ITRMAX    maximum number of iterations (default 100)
   */
  class NewtonRaphson(val xstart : Double, val df : Double => Double, val d2f : Double => Double, val tol : Double = 1e-6, val ITRMAX : Int = 100) {
    
    /** The convergent iterator used to run the decesent, you can obtain MaximumIterations exception through this */
    val convergentiteration = new ConvergentIteration[Double](xstart, x=>x-df(x)/d2f(x), (x,y)=>(x-y).abs<tol, ITRMAX)
    /** The Double x that minimises your function */
    lazy val xmin = convergentiteration.limit
    
  }

}
