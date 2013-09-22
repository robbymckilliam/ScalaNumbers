/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package numbers.matrix.field

import numbers.finite.Complex
import numbers.IntegerMatrix
import numbers.Integer
import numbers.finite.ComplexMatrix
import numbers.finite.RectComplex
import numbers.finite.PolarComplex
import numbers.finite.Real
import numbers.finite.RealMatrix
import scala.collection.mutable.ArraySeq
import org.junit.Test
import org.junit.Assert._


class MatrixTest {

  val tol = 1e-6
  
  def diff(x : Complex, y : Complex) = (x-y).magnitude
  
  @Test
  def complexElementTest() {
    val N = 5
    val M = 4
    def f(m : Int, n : Int) = new RectComplex(n, m)
    val A = new ComplexMatrix(f,M,N)
    for(m <- 0 until M; n <- 0 until N) assertTrue(diff(f(m,n),A(m,n)) <= tol)
  }
  
  @Test
  def integerElementTest() {
    val N = 5
    val M = 4
    def f(m : Int, n : Int) = Integer(n*m)
    val A = new IntegerMatrix(f,M,N)
    for(m <- 0 until M; n <- 0 until N) assertTrue(f(m,n)==A(m,n))
  }
  
  @Test
  def submatrixTest() {
    val N = 5
    val M = 4
    def f(m : Int, n : Int) = Integer(n*m)
    val A = new IntegerMatrix(f,M,N)
    val ms = Array(1,2)
    val ns = Array(1,4)
    val B = A.submatrix(ms, ns)
    assertTrue(B(0,0)==Integer(1)); assertTrue(B(0,1)==Integer(4)); 
    assertTrue(B(1,0)==Integer(2)); assertTrue(B(1,1)==Integer(8)); 
  }
  
  @Test
  def rowTest() {
    val N = 5
    val M = 4
    def f(m : Int, n : Int) = Integer(n*m)
    val A = new IntegerMatrix(f,M,N)
    val m = 1
    val B = A.row(m)
    for( n <- 0 until N) assertTrue(B(0,n)==A(m,n))
  }
  
  @Test
  def columnTest() {
    val N = 5
    val M = 4
    def f(m : Int, n : Int) = Integer(n*m)
    val A = new IntegerMatrix(f,M,N)
    val n = 1
    val B = A.column(n)
    for( m <- 0 until M) assertTrue(B(m,0)==A(m,n))
  }
  
  @Test
  def addTest() {
    val N = 5
    val M = 4
    def f(m : Int, n : Int) = Integer(n*m)
    val A = new IntegerMatrix(f,M,N)
    val B = A+A
    for( m <- 0 until M; n <- 0 until N) assertTrue(B(m,n)==A(m,n)+A(m,n))
  }
  
  @Test
  def subtractTest() {
    val N = 5
    val M = 4
    def f(m : Int, n : Int) = Integer(n*m)
    val A = new IntegerMatrix(f,M,N)
    val B = A-A
    for( m <- 0 until M; n <- 0 until N) assertTrue(B(m,n)==Integer(0))
  }
  
  @Test
  def scalaMultTest() {
    val N = 5
    val M = 4
    def f(m : Int, n : Int) = Integer(n*m)
    val A = new IntegerMatrix(f,M,N)
    val B = A*Integer(2)
    for( m <- 0 until M; n <- 0 until N) assertTrue(B(m,n)==Integer(2*n*m))
  }
  
  @Test
  def transposeTest() {
    val N = 5
    val M = 4
    def f(m : Int, n : Int) = Integer(n*m)
    val A = new IntegerMatrix(f,M,N)
    val B = A.transpose
    for( m <- 0 until M; n <- 0 until N) assertTrue(B(n,m)==A(m,n))
  }
  
  @Test
  def matrixMultTest() {
    val N = 2
    val M = 2
    def f(m : Int, n : Int) = Integer(n*m+1)
    val A = new IntegerMatrix(f,M,N)
    val B = A*A
    assertTrue(B(0,0)==Integer(2)); assertTrue(B(0,1)==Integer(3))
    assertTrue(B(1,0)==Integer(3)); assertTrue(B(1,1)==Integer(5))
  }
  
  @Test
  def matrixMult2Test() {
    val N = 3
    val M = 2
    def f(m : Int, n : Int) = Integer(n*m+1)
    val A = new IntegerMatrix(f,M,N)
    val B = A*A.transpose
    assertTrue(B(0,0)==Integer(3)); assertTrue(B(0,1)==Integer(6))
    assertTrue(B(1,0)==Integer(6)); assertTrue(B(1,1)==Integer(14))
  }
  
  @Test
  def backwithArrayTest() {
    val N = 3
    val M = 2
    def f(m : Int, n : Int) = Integer(n*m-10)
    val A = new IntegerMatrix(f,M,N)
    val B = A.backwitharray
    for( m <- 0 until M; n <- 0 until N) assertTrue(B(m,n)==A(m,n))
  }
  
  @Test
  def svdRealTest() {
    val tol = 1e-6
    val N = 3
    val M = 2
    def f(m : Int, n : Int) = Real(n*m+1)
    val A = new RealMatrix(f,M,N)
    val (u,s,v) = A.svd
    val B = u*s*v.transpose
    for( m <- 0 until M; n <- 0 until N) assertEquals(B(m,n).d,A(m,n).d,tol)
    
    //test versus some output from Matlab
    def fu(m : Int, n : Int) : Real = {
      if( (m,n)==(0,0) ) Real(-0.402663241110145)
      else if( (m,n)==(0,1) ) Real(-0.915348192907307)
      else if( (m,n)==(1,0) ) Real(-0.915348192907307)
      else if( (m,n)==(1,1) ) Real(0.402663241110145)
      else Real.zero
    }
    val matlabU = new RealMatrix(fu,M,M)
    for( m <- 0 until M; n <- 0 until M) assertEquals(matlabU(m,n).d,u(m,n).d,tol)
  }
  
  @Test
  def svdComplexTest() {
    val tol = 1e-6
    val N = 3
    val M = 2
    def f(m : Int, n : Int) = new RectComplex(n,m)
    val A = new ComplexMatrix(f,M,N)
    val (u,s,v) = A.svd
    val B = u*s*v.hermitianTranspose
    for( m <- 0 until M; n <- 0 until N) assertTrue(diff(B(m,n),A(m,n)) < tol)
    
    //test versus some output from Matlab
    def fu(m : Int, n : Int) : Complex = {
      if( (m,n)==(0,0) ) new RectComplex(-0.612724881345119, 0.0)
      else if( (m,n)==(0,1) ) new RectComplex(0.79029628607289, 0.0)
      else if( (m,n)==(1,0) ) new RectComplex(-0.677673474524404, -0.406604084714642)
      else if( (m,n)==(1,1) ) new RectComplex(-0.525407251161498, -0.315244350696899)
      else Complex.zero
    }
    val matlabU = new ComplexMatrix(fu,M,M)
    for( m <- 0 until M; n <- 0 until M) assertTrue(diff(matlabU(m,n),u(m,n)) < tol)
  }
  
  @Test
  def complexInverseTest() {
    val N = 4
    def f(m : Int, n : Int) = new RectComplex(scala.util.Random.nextDouble,scala.util.Random.nextDouble)
    val A = new ComplexMatrix(f,N,N).backwitharray
    val B = A.inverse
    val I = A.identity(N)
    val C = A*B
    val D = B*A
    for( m <- 0 until N; n <- 0 until N) assertTrue(diff(C(m,n),I(m,n)) < tol)
    for( m <- 0 until N; n <- 0 until N) assertTrue(diff(D(m,n),I(m,n)) < tol)
  }
  
  @Test
  def complexPsuedoInverseTest() {
    val M = 3; val N = 4
    def f(m : Int, n : Int) =  new RectComplex(scala.util.Random.nextDouble,scala.util.Random.nextDouble)
    val A = new ComplexMatrix(f,M,N).backwitharray
    val B = A.pinv
    assertTrue(((A*B*A) - A).frobeniusNorm < tol)
    assertTrue(((B*A*B) - B).frobeniusNorm < tol)
    assertTrue(((A*B).h - (A*B)).frobeniusNorm < tol)
    assertTrue(((B*A).h - (B*A)).frobeniusNorm < tol)
  }
  
  @Test
  def indicesTest() {
    val N = 2
    val M = 3
    def f(m : Int, n : Int) = Integer(n*m)
    val A = new IntegerMatrix(f,M,N)
    val sum = A.indices.foldLeft(Integer.zero)( (v, i) => v + A(i) )
    assertTrue(sum==Integer(3))  
  }
  
  @Test
  def complexFrobeniusNormTest() {
    val tol = 1e-8
    val N = 2
    val M = 3
    val A = new ComplexMatrix( (m,n) => new PolarComplex(1, 0.4), M,N)
    assertEquals( scala.math.sqrt(1.0*M*N), A.frobeniusNorm, tol )
  }
  
  @Test
  def realQRIdentityTest() {
    val tol = 1e-8
    val N = 3
    val M = 3
    val I = RealMatrix.identity(N)
    val (q,r) = I.qr
    for( m <- 0 until M; n <- 0 until N) assertEquals(q(m,n).d,I(m,n).d,tol)
    for( m <- 0 until M; n <- 0 until N) assertEquals(r(m,n).d,I(m,n).d,tol)
  }
  
  @Test
  def realQRColumnTest () {
    val tol = 1e-8
    val M = 9
    val N = 1
    val m = new RealMatrix( (m,n) => Real.one, M,N )
    val (q,r) = m.qr
    val rexp = RealMatrix( (m,n) => if(m==0) Real(3.0) else Real(0.0), M,N)
    assertTrue((r - rexp).frobeniusNorm < tol)
    assertTrue((q*r - m).frobeniusNorm < tol)
  }
  
  def realQRTest () {
    val tol = 1e-8
    val M = 3
    val N = 3
    val marray = ArraySeq(ArraySeq(1.0, 1.0, 1.0),
                            ArraySeq( 1.0, 2.0, 3.0),
                            ArraySeq( 1.0, 4.0, 9.0))
    val m = RealMatrix.fromDoubleArray(marray)
    val (q,r) = m.qr
    println(m)
    println(r)
    println(q)
    println(q*r)
    val rexparray = ArraySeq(ArraySeq(1.732050807568877, 4.041451884327381, 7.505553499465135), //output from matlab
                            ArraySeq( 0.0, 2.160246899469286, 5.863527298559493),
                            ArraySeq( 0.0, 0.0, 0.534522483824849))
    val rexp = RealMatrix.fromDoubleArray(rexparray)
    val qexparray = ArraySeq(ArraySeq( 0.577350269189626, -0.617213399848368, 0.534522483824849), //output from matlab
                            ArraySeq(0.577350269189626, -0.154303349962092, -0.801783725737273),
                            ArraySeq(0.577350269189626, 0.771516749810460, 0.267261241912424))
    val qexp = RealMatrix.fromDoubleArray(qexparray)
    assertTrue((r - rexp).frobeniusNorm < tol)
    assertTrue((q - qexp).frobeniusNorm < tol)
    assertTrue((q*r - m).frobeniusNorm < tol)
    assertTrue((q*q.transpose - RealMatrix.identity(M)).frobeniusNorm < tol)
  }

  def complexQRIdentityTest() {
    val tol = 1e-8
    val N = 3
    val M = 3
    val I = ComplexMatrix.identity(N)
    val (q,r) = I.qr
    //println(q)
    //println(r)
    for( m <- 0 until M; n <- 0 until N) assertTrue((q(m,n)-I(m,n)).magnitude < tol)
    for( m <- 0 until M; n <- 0 until N) assertTrue((r(m,n)-I(m,n)).magnitude < tol)
  }
  
  @Test
  def complexQRColumnTest () {
    val tol = 1e-8
    val M = 9
    val N = 1
    val m = new ComplexMatrix( (m,n) => Complex.one, M,N )
    val (q,r) = m.qr
    val rexp = ComplexMatrix( (m,n) => if(m==0) Complex.one*3.0 else Complex.zero, M,N)
    assertTrue((r - rexp).frobeniusNorm < tol)
    assertTrue((q*r - m).frobeniusNorm < tol)
  }
  
  @Test
  def largeQRTest () {
    val tol = 1e-8
    val M = 30
    val N = 30
    val m = new RealMatrix( (m,n) => Real(scala.util.Random.nextDouble), M,N ).backwitharray
    val (q,r) = m.qr
    //println(q)
    //println(r)
    assertTrue((q*r - m).frobeniusNorm < tol)
  }
  
  @Test
  def largeComplexQRTest () {
    val tol = 1e-8
    val M = 30
    val N = 30
    val m = new ComplexMatrix( (m,n) => RectComplex(scala.util.Random.nextDouble,scala.util.Random.nextDouble), M,N ).backwitharray
    val (q,r) = m.qr
    //println(q)
    //println(r)
    //println(q*r)
    //println(m)
    assertTrue((q*r - m).frobeniusNorm < tol)
  }
  
}
