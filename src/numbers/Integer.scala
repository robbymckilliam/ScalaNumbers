/*
 * Implements integers
 * @author Robby McKilliam
 */

package numbers

import bignums.BigInteger
import numbers.matrix.MatrixWithElementsFromAEuclideanDomain

object Integer {
  val one : Integer = Integer(1)
  val zero : Integer = Integer(0)
  
  /** Contructors from various other values, Int, Long, BigInt, BigInteger */
  def apply(x : BigInteger) : Integer = new Integer(x)
  def apply(x : Int) : Integer = Integer(new BigInteger(x.toString))
  def apply(x : Long) : Integer = Integer(new BigInteger(x.toString))
  def apply(x : BigInt) : Integer = Integer(new BigInteger(x.toString))
  implicit def toInteger(i : Int) = Integer(i)
  implicit def toInteger(i : Long) = Integer(i)
  implicit def toInteger(i : BigInt) = Integer(i)
}

/** 
 * Class describing integers.
 * Internally uses a bignums.BigInteger.  This is much faster than java.BigInteger (and correspondingly)
 * scala's BigInt for very larger integers.  If java.BigInteger ever gets fixed then this could be changed.
 */
class Integer(val bigint : BigInteger) extends EuclideanDomain[Integer, Integer] with Ordered[Integer] {
  
  override def +(that : Integer) : Integer = Integer(bigint.add(that.bigint))
  def +(that : Int) : Integer = this + Integer(that)
  override def unary_- : Integer = Integer(bigint.negate)
  override def -(that : Integer) : Integer = Integer(bigint.subtract(that.bigint))
  def -(that : Int) : Integer = this - Integer(that)
  override def *(that : Integer) : Integer = Integer(bigint.multiply(that.bigint))
  def *(that : Int) : Integer = this * Integer(that)
  
  /** Not implemented */
  override def factors : Seq[Integer] = throw new UnsupportedOperationException("Not implemented")
  
  override def norm : Integer = new Integer(bigint.abs)
  def abs : Integer = norm
  override def / (that : Integer) : Integer = new Integer(bigint.divide(that.bigint))
  override def mod (that : Integer) : Integer = {
    if(that>Integer.zero) return new Integer(bigint.mod(that.bigint))
    else if(that<Integer.zero) return -((-this)mod(-that))
    else throw new RuntimeException("You tried to compute " + this + " mod 0.  That is not well defined silly!")
  }
//  override def mod (that : Integer) : Integer = new IntegerFrombignumsBigInteger(bigint.mod(that.bigint))
  
  final override def zero : Integer = Integer.zero
  final override def one : Integer = Integer.one
  
  final override def ==(that : Integer) : Boolean = bigint.equals(that.bigint)
  final def ==(that : Int) : Boolean = this == Integer(that)
  final def ==(that : Long) : Boolean = this == Integer(that)
  final def ==(that : BigInt) : Boolean = this == Integer(that)
  
  final override def compare(that : Integer) : Int = bigint.compareTo(that.bigint)
  
  override def toString : String = bigint.toString
  
}

object IntegerMatrix {
  /// contruct identity matrix
  def identity(M : Int, N : Int) : IntegerMatrix = new IntegerMatrix ( (m,n) => if(m==n) Integer.one else Integer.zero, M,N)
  def identity(N : Int): IntegerMatrix = identity(N,N)
  def apply(f : (Int,Int) => Integer, M : Int, N : Int) = construct(f,M,N)
  def construct(f : (Int,Int) => Integer, M : Int, N : Int) = new IntegerMatrix(f,M,N)
  def constructRow(f : (Int) => Integer, N : Int) = construct( (m,n) => f(n), 1, N)
  def constructColumn(f : (Int) => Integer, M : Int) = construct( (m,n) => f(m), M, 1)
}

/** A matrix containing integers */
class IntegerMatrix( val f : (Int,Int) => Integer, override val M : Int, override val N : Int)  
  extends MatrixWithElementsFromAEuclideanDomain[Integer, IntegerMatrix] {
    
  override protected def get(m : Int, n : Int) = f(m,n)
  override def construct(f : (Int,Int) => Integer, M : Int, N : Int) = IntegerMatrix.construct(f,M,N)
  
  override def smithNormalForm = throw new UnsupportedOperationException("not implemented yet")
  override def hermiteNormalForm = throw new UnsupportedOperationException("not implemented yet")
  
  /** 
   *Returns the determinant of this matrix.
   *NOTE: Currently this just converts the matrix to RealMatrix to do this, but probably the Hermite normal
   *form should be used instead.
   */
  lazy val determinant = {
    //if(N!=M) throw new ArrayIndexOutOfBoundsException("Only square matrices have determinants!")
    val rationaldet = RationalMatrix( (m,n) => Rational(this(m,n),Integer.one), M,N).det
    rationaldet.n
  }
  lazy val det = determinant
  
}