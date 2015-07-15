package numbers.finite.optimisation

import numbers.finite.optimisation.SingleVariableOptimisation.fmin
import numbers.finite.optimisation.SingleVariableOptimisation.fmax
import numbers.finite.optimisation.SingleVariableOptimisation.fzero
import numbers.finite.RealMatrix
import numbers.finite.Real

import org.junit.Test;
import org.junit.Assert._;

class OptimisationTest {

  val tol = 1e-7
  
  def sqr(x : Double) = x*x
  def sqr(x : Real) = x*x
	
  @Test
  def QuadraticTest = {
    val f : Double => Double = x => (x-2.0)*(x-2.0)
    val (fxm, xm) = fmin(f, -4.0,1.0,5.0, tol)
    assertEquals(0.0, fxm, tol)
    assertEquals(2.0, xm, tol)
  }
  
  @Test
  def QuarticTest = {
    val f : Double => Double = x => x*x*x*x
    val (fxm, xm) = fmin(f, -2.0, 1.0, 2.0, tol)
    assertEquals(0.0, fxm, tol)
    assertEquals(0.0, xm, tol)
    
    val (fxmd, xmd) = fmin(f, -10.0,3.0, 9.0)
    assertEquals(0.0, fxmd, 1e-6)
    assertEquals(0.0, xmd, 1e-6)
    
  }
  
  @Test
  def fmaxTest = {
    val f : Double => Double = x => -(x-2)*(x-2)
    val (fxm, xm) = fmax(f, -11.0,4.0,8.0,tol)
    assertEquals(0.0, fxm, tol)
    assertEquals(2.0, xm, tol)
    
    val (fxmd, xmd) = fmax(f, -11.0,4.0,13.0)
    assertEquals(0.0, fxmd, 1e-6)
    assertEquals(2.0, xmd, 1e-6)    
  }
  
  @Test
  def fzeroLinearTest = {
    val f : Double => Double = x => x
    val x = fzero(f, -11.0,8.0,tol)
    assertEquals(0.0, f(x), tol)
    assertEquals(0.0, x, tol)
  }
  
  @Test
  def fzeroCubicTest = {
    val f : Double => Double = x => x*x*(x-1)
    val x1 = fzero(f, 0.5,1.7,tol)
    println(x1,f(x1))
    //assertEquals(0.0, f(x1), tol)
    assertEquals(1.0, x1, tol)
    
    //THIS CASE FAILS BECAUSE FZERO USES BISCECTION!
    //val x2 = fzero(f, -0.5,0.7,tol)
    //println(x2,f(x2))
    //assertEquals(0.0, f(x2), tol)
    //assertEquals(0.0, x2, tol)
  }
  
  @Test
  def gradientdescentTest = {
    val f : Double => Double = x => (x-2)*(x-2)
    val df : Double => Double = x => 2*(x-2)
    val xstart = 10.0
    val xmin = new SingleVariableOptimisation.GradientDescent(xstart, df).xmin
    assertEquals(2.0, xmin, 1e-5)
    
    val f2 : Double => Double = x => x*(x+1)*(x-2)
    val df2 : Double => Double = x => x*(3*x-2)-2
    val xstart2 = 0.0
    val xmin2 = new SingleVariableOptimisation.GradientDescent(xstart2, df2).xmin
    assertEquals(1.21525, xmin2, 1e-4)
  }
  
  @Test
  def multivariableGradientDescentTest = {
    val L = 5 //dimension
    val f : RealMatrix => Double = x => x.squaredFrobeniusNorm.d
    val df : RealMatrix => RealMatrix = x => x * 2.0 //elementwise multiplication by 2
    val xstart = RealMatrix.constructRow( n => Real(2.0*n), L) //L variable optimisation
    val xmin = new MultiVariableOptimisation.GradientDescent(xstart, df).xmin
    assertTrue( xmin.frobeniusNorm.d.abs < 1e-5 )
  }
  
  @Test
  def NewtonRaphsonTest = {
    val f : Double => Double = x => (x-2)*(x-2)
    val df : Double => Double = x => 2*(x-2)
    val d2f : Double => Double = x => 2
    val xstart = 10.0
    val xmin = new SingleVariableOptimisation.NewtonRaphson(xstart, df, d2f).xmin
    assertEquals(2.0, xmin, 1e-5)
    
    val f2 : Double => Double = x => x*(x+1)*(x-2)
    val df2 : Double => Double = x => x*(3*x-2)-2
    val d2f2 : Double => Double = x => (3*x-2) + 3*x
    val xstart2 = 1.0
    val xmin2 = new SingleVariableOptimisation.NewtonRaphson(xstart2, df2, d2f2).xmin
    assertEquals(1.21525, xmin2, 1e-4)
  }
  
  @Test
  def multivariateNewtonRaphsonTest = {
    val L = 5 //dimension
    val f : RealMatrix => Double = x => x.squaredFrobeniusNorm.d
    val df : RealMatrix => RealMatrix = x => x * 2.0 //elementwise multiplication by 2
    val H : RealMatrix => RealMatrix = x => RealMatrix.identity(L)*2 
    val xstart = RealMatrix.constructRow( n => Real(2.0*n), L) //L variable optimisation
    val xmin = new MultiVariableOptimisation.NewtonRaphson(xstart, df, H).xmin
    assertTrue( xmin.frobeniusNorm.d.abs < 1e-5 )
  }
    
  /// Test the NewtonRaphson method with the Rosenbrock function (1-x)^2 + 100(y-x)^2 
  @Test def RosenbrockFunctionNewtonRaphson = {
    def rosenbrock(x : Double, y : Double) = sqr(1-x) + 100*sqr(y-x*x)
    val f : RealMatrix => Double = x => rosenbrock(x(0).d,x(1).d)
    val df : RealMatrix => RealMatrix = x => RealMatrix.constructRow(n => Real(gradrosenbrock(n,x(0).d,x(1).d)), 2)
    val H : RealMatrix => RealMatrix = x => new RealMatrix( (m,n) => Real(hessianrosenbrock(m,n,x(0).d,x(1).d)), 2,2)
    val xstart = RealMatrix.asRow( Array(Real(1.08),Real(1.11111)) )
    val xmin = new MultiVariableOptimisation.NewtonRaphson(xstart, df, H).xmin
    assertEquals(1.0, xmin(0).d, 1e-5) 
    assertEquals(1.0, xmin(1).d, 1e-5) 
  }
  
  /** 
   * The Gauss-Newton method can be implemented using NewtonRaphson by replacing
   * the full Hessian with an approximate one.  Here, we test this with the Rosenbrock
   * function (1-x)^2 + 100(y-x)^2.  In this case on a single term is removed from the Hessian
   */
  @Test def RosenbrockFunctionGaussNewton = {
    def rosenbrock(x : Double, y : Double) = sqr(1-x) + 100*sqr(y-x*x)
    val f : RealMatrix => Double = x => rosenbrock(x(0).d,x(1).d)
    val df : RealMatrix => RealMatrix = x => RealMatrix.constructRow(n => Real(gradrosenbrock(n,x(0).d,x(1).d)), 2)
    val H : RealMatrix => RealMatrix = x => new RealMatrix( (m,n) => Real(approxhessianrosenbrock(m,n,x(0).d)), 2,2)
    val xstart = RealMatrix.asRow( Array(Real(1.1469706840390879),Real(1.3110566775244301)) )
    val xmin = new MultiVariableOptimisation.NewtonRaphson(xstart, df, H).xmin
    println("xmin = " + xmin)
    assertEquals(1.0, xmin(0).d, 1e-5) 
    assertEquals(1.0, xmin(1).d, 1e-5)
  }
  
  /// Gradient of the Rosenbock function
  def gradrosenbrock(n : Int, x : Double, y : Double) : Double = {
    if(n==0) return -(1-x)*2-(y-x*x)*400 
    if(n==1) return (y-x*x)*200
    throw new ArrayIndexOutOfBoundsException("Rosenbrock only has two variables")
  }
  
  /// Hessian of the Rosenbock function
  def hessianrosenbrock(m : Int, n : Int, x : Double, y : Double) : Double = {
    if(n==0 && m==0) return 2 + 800*x*x - 400*(y-x*x)
    if((n==1 && m==0) || (n==0 & m == 1)) return -400*x
    if(n==1 && m==1) return 200
    else throw new ArrayIndexOutOfBoundsException("Rosenbrock only has two variables")
  }
    
  /// Approximate Hessian used for Gauss-Newton of the Rosenbock function
  def approxhessianrosenbrock(m : Int, n : Int, x : Double) : Double = {
    if(n==0 && m==0) return 2 + 800*x*x
    if((n==1 && m==0) || (n==0 & m == 1)) return -400*x
    if(n==1 && m==1) return 200
    else throw new ArrayIndexOutOfBoundsException("Rosenbrock only has two variables")
  }
  
  ///Test the Rosenbock hessians
  @Test def approxhessianrosenbrockTest = {
    val H : Double => RealMatrix = x => new RealMatrix( (m,n) => Real(approxhessianrosenbrock(m,n,x)), 2,2)
    //the approximate Hessian always has det 400
    for( x <- -2.0 to 2.0 by 0.1 ) assertEquals(400.0, H(x).det.d, 1e-8)
  }
 
  
//  ///Test a few manual iterations of the Gauss-Newton method on the Rosenbock function
//  @Test def manualGaussNewtonRosenbock = {
//    val df : RealMatrix => RealMatrix = x => RealMatrix.constructRow(n => Real(gradrosenbrock(n,x(0).d,x(1).d)), 2)
//    val H : RealMatrix => RealMatrix = x => new RealMatrix( (m,n) => Real(approxhessianrosenbrock(m,n,x(0).d)), 2,2)
//    val step : RealMatrix => RealMatrix = x=>(x - df(x)*H(x).inv).backwitharray
//    val xstart = RealMatrix.asRow( Array(Real(1.1469706840390879),Real(1.3110566775244301)) )
//    var x = xstart
//    for( n <- 1 to 10 ) {
//      println(x)
//      x = step(x)
//    }
//  }
//  
//  ///Test a few manual iterations of the Gauss-Newton method on the Rosenbock function
//  @Test def manualNewtonRaphsonRosenbock = {
//    val df : RealMatrix => RealMatrix = x => RealMatrix.constructRow(n => Real(gradrosenbrock(n,x(0).d,x(1).d)), 2)
//    val H : RealMatrix => RealMatrix = x => new RealMatrix( (m,n) => Real(hessianrosenbrock(m,n,x(0).d,x(1).d)), 2,2)
//    val step : RealMatrix => RealMatrix = x=>(x - df(x)*H(x).inv).backwitharray
//    val xstart = RealMatrix.asRow( Array(Real(1.1469706840390879),Real(1.3110566775244301)) )
//    var x = xstart
//    for( n <- 1 to 10 ) {
//      println(x)
//      x = step(x)
//    }
//  }
  
}