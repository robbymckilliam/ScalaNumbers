/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package numbers.finite

import numbers.Field
import numbers.matrix.MatrixWithElementsFromAField
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
   * where V' is the transpose of V.  Uses the JBlas library, which is very fast but requires some
   * native code. Ultimately, it would be better to have a pure java or scala version of this.
   */
  def singularValueDecomposition : (RealMatrix, RealMatrix, RealMatrix) = {
    val A = org.jblas.DoubleMatrix.zeros(M,N)
    for( m <- 0 until M; n <- 0 until N ) A.put(m,n,this(m,n).d)
    val USV = org.jblas.Singular.sparseSVD(A)
    val U = construct( (m,n) => Real(USV(0).get(m,n)), USV(0).rows, USV(0).columns )
    val S = construct( (m,n) => if(m==n) Real(USV(1).get(m,0)) else Real.zero, USV(1).rows, USV(1).rows )
    val V = construct( (m,n) => Real(USV(2).get(m,n)), USV(2).rows, USV(2).columns )
    return (U,S,V)
  }
  final def svd = singularValueDecomposition
  
  def qr = throw new UnsupportedOperationException("not implemented yet")
  override def smithNormalForm = singularValueDecomposition
  override def hermiteNormalForm = qr

}