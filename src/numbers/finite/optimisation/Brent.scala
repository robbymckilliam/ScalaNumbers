/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package numbers.finite.optimisation

import scala.math.signum
import scala.math.abs
import scala.math.sqrt
import scala.util.control.Breaks._

object SingleVariableOptimisation {
  
  val defaultol = 1e-6
  
  /** 
   *Search for the minimum of the function f(x) in the interval [a, b].
   *Finds a solution such that |x0 - x| < tol where x0 is the minimiser of f
   */
  def fmin(f : Double => Double, a : Double, b: Double, tol : Double) : (Double, Double) = {
    val bx = if(a >= b) 0.5*(a - b) else 0.5*(b - a)
    return new Brent(f,a,bx,b,tol).min
  }
  
  /** Run fmin with default tolerance of 1e-6 */
  def fmin(f : Double => Double, a : Double, b: Double) : (Double, Double) = fmin(f,a,b,defaultol) 
  
  /** 
   *Search for the maximum of the function f(x) in the interval [a, b].
   *Finds a solution such that |x0 - x| < tol where x0 is the minimiser of f
   */
  def fmax(f : Double => Double, a : Double, b: Double, tol : Double) : (Double, Double) = {
    return fmin(-f(_),a,b,tol)
  }
  
  /** Run fmax with default tolerance of 1e-6 */
  def fmax(f : Double => Double, a : Double, b: Double) : (Double, Double) = fmax(f,a,b,defaultol) 
  
}

/**
 *Performs a 1-dimensional minimization.
 *It implements Brent's method which combines a golden-section
 *search and parabolic interpolation.  
 *
 * Ported from Numerical Recipes in C pages 404 - 405
 *
 *@param f           The function to minimise 
 *@param  ax         Left endpoint of initial interval
 *@param  cx         Right endpoint of initial interval
 *@param  bx         bx must satisfy ax < bx < cx and f(ax) > f(bx) < f(cx)
 *@param  tol       Desired length of the interval in which
 *                   the minimum will be determined to lie
 *                   (This should be greater than, roughly, 3.0e-8.)
 */
class Brent(f : Double => Double, ax : Double, bx : Double, cx: Double, tol : Double) {
  val ITRMAX = 100 //maximum number of iterations
  val ZEPS = 1e-10 //close to machine precision
  val C = (3.0 - sqrt(5.0))/2.0;
  
  val (fmin, xmin) = run //run the optimiser
  
  /** Return (f(x), x) where f(x) is the minimum */
  def min = (fmin, xmin)
  
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
    println("Warning: Brent's method reached maximum number of 100 iterations")
    return (fx, x)
  }
  
  protected final def sign(a : Double, b : Double) = abs(a)*signum(b)
  
}
