/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package numbers.finite

import numbers.Field
import numbers.matrix.MatrixWithElementsFromAField
import org.netlib.lapack.Dgesvd
import scala.math.min
import scala.math.max

/**
 * Provides static definitions of the multiplicative and additive identities.
 * This is a good candidate for scala 2.10's value classes.
 */
object Real {
  val one = new Real(1.0) 
  val zero = new Real(0.0)
  
  def apply(x : Double) = new Real(x)
}

class Real(val d : Double) extends Field[Real, Real] with Ordered[Real] {
  //@inline final def toDouble = d
  
  final def - = new Real(-d)
  final def +(that: Real) = new Real(d + that.d)
  final def -(that: Real) = new Real(d - that.d)
  final def +(that: Double) : Real = new Real(d + that)
  final def -(that: Double) : Real = new Real(d - that)
  final def +(that: Int) : Real = new Real(d + that)
  final def -(that: Int) : Real = new Real(d - that)

  final def *(that: Real) = new Real(d * that.d)
  final def /(that: Real) = new Real(d / that.d)
  final def *(that: Double) : Real = new Real(d * that)
  final def /(that: Double) : Real = new Real(d / that)
  final def / : Real = new Real(1.0 / d)
  final def *(that: Int) : Real = new Real(d * that)
  final def /(that: Int) : Real = new Real(d / that)
  
  final def one : Real = Real.one
  final def zero : Real = Real.zero
    
  final def norm : Real = new Real(d.abs)
    
  /// Don't factorise the real numbers!
  final def factors = throw new UnsupportedOperationException("Don't factorise the real numbers!")
  
  final def ==(that : Real) = this.d == that.d
  final def ==(that : Double) = this.d == d
  final def !=(that : Real) = !(this == that)
  final def !=(that : Double) = !(this == that)
  
  /** Uses scala's internal Ordered, only need to override compare */
  final def compare(that : Real) : Int = this.d.compare(that.d)
  
  final override def toString : String  = d.toString
}


/** Matrix with real elements */
class RealMatrix(f : (Int,Int) => Real, override val M : Int, override val N : Int) 
extends MatrixWithElementsFromAField[Real, RealMatrix] {
    
  override def apply(mn : (Int,Int)) = f(mn._1,mn._2)
  override def construct(f : (Int,Int) => Real, M : Int, N : Int) = new RealMatrix(f,M,N)
  
  /** 
   * Returns (U, S, V) where U is MxM and V is NxN, both orthonormal and S the diagonal matrix such that
   *  this = U S V'
   * where V' is the transpose of V.  Uses the JLapack library.
   */
  def singularValueDecomposition : (RealMatrix, RealMatrix, RealMatrix) = {
    val A = new Array[Double](M*N)
    for( n <- 0 until N ; m <- 0 until M ) A(m + M*n) = this(m,n).d
    val s = new Array[Double](M)
    val u = new Array[Double](M*M)
    val vt = new Array[Double](N*N)
    val work = new Array[Double](max(3*min(M,N)+max(M,N),5*min(M,N)))
    val info = new org.netlib.util.intW(2)
    Dgesvd.dgesvd("A","A",M,N,A, 0,M,s, 0,u, 0,M,vt, 0, N, work, 0, work.length, info)
    //if(info.val != 0)  println("singular value decomposition do not converge!")
    val S = construct( (m,n) => if(n==m) Real(s(m)) else Real.zero, M, M )
    val U = construct( (m,n) => Real(u(m+M*n)), M, M )
    val V = construct( (m,n) => Real(vt(n+N*m)), N, N ) //Lapack puts out transpose, we reverse that here.
    return (U, S, V)
  }
  @inline final def svd = singularValueDecomposition
  
  def qr = throw new UnsupportedOperationException("not implemented yet")
  override def smithNormalForm = singularValueDecomposition
  override def hermiteNormalForm = qr

}