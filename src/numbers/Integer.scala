/*
 * Implements integers
 * @author Robby McKilliam
 */

package numbers

import bignums.BigInteger
import numbers.matrix.MatrixWithElementsFromAEuclideanDomain

object Integer {
  val one : Integer = new IntegerFromInt(1)
  val zero : Integer = new IntegerFromInt(0)
  
  /** Contructors from various other values, Int, Long, BigInt, BigInteger */
  def apply(x : Int) : Integer = new IntegerFromInt(x)
  def apply(x : Long) : Integer = new IntegerFromLong(x)
  def apply(x : BigInt) : Integer = new IntegerFromBigInt(x)
  def apply(x : BigInteger) : Integer = new IntegerFrombignumsBigInteger(x)
}

/** 
 * Abstract class describing integers.
 * Internally uses a bignums.BigInteger.  This is much faster than java.BigInteger (and correspondingly)
 * scala's BigInt for very larger integers.  If java.BigInteger ever gets fixed then this could be changed.
 */
protected class Integer(val bigint : BigInteger) extends EuclideanDomain[Integer, Integer] with Ordered[Integer] {
  
  override def +(that : Integer) : Integer = new IntegerFrombignumsBigInteger(bigint.add(that.bigint))
  def +(that : Int) : Integer = this + new IntegerFromInt(that)
  override def unary_- : Integer = new IntegerFrombignumsBigInteger(bigint.negate)
  override def -(that : Integer) : Integer = new IntegerFrombignumsBigInteger(bigint.subtract(that.bigint))
  def -(that : Int) : Integer = this - new IntegerFromInt(that)
  override def *(that : Integer) : Integer = new IntegerFrombignumsBigInteger(bigint.multiply(that.bigint))
  def *(that : Int) : Integer = this * new IntegerFromInt(that)
  
  /** Not implemented */
  override def factors : Seq[Integer] = throw new UnsupportedOperationException("Not implemented")
  
  override def norm : Integer = new IntegerFrombignumsBigInteger(bigint.abs)
  def abs : Integer = norm
  override def / (that : Integer) : Integer = new IntegerFrombignumsBigInteger(bigint.divide(that.bigint))
  override def mod (that : Integer) : Integer = {
    if(that>Integer.zero) return new IntegerFrombignumsBigInteger(bigint.mod(that.bigint))
    else if(that<Integer.zero) return -((-this)mod(-that))
    else throw new RuntimeException("You tried to compute " + this + " mod 0.  That is not well defined silly!")
  }
//  override def mod (that : Integer) : Integer = new IntegerFrombignumsBigInteger(bigint.mod(that.bigint))
  
  final override def zero : Integer = Integer.zero
  final override def one : Integer = Integer.one
  
  final override def ==(that : Integer) : Boolean = bigint.equals(that.bigint)
  
  final override def compare(that : Integer) : Int = bigint.compareTo(that.bigint)
  
  override def toString : String = bigint.toString
  
}

protected class IntegerFromInt(val x : Int) extends Integer(new BigInteger(x.toString))
protected class IntegerFromLong(val x : Long) extends Integer(new BigInteger(x.toString))
protected class IntegerFromBigInt(val x : BigInt) extends Integer(new BigInteger(x.toString))
protected class IntegerFrombignumsBigInteger(val x : BigInteger) extends Integer(new BigInteger(x.toString))

/** A matrix containing integers */
class IntegerMatrix( val f : (Int,Int) => Integer, override val M : Int, override val N : Int)  
  extends MatrixWithElementsFromAEuclideanDomain[Integer, IntegerMatrix] {
    
  override protected def get(m : Int, n : Int) = f(m,n)
  override def construct(f : (Int,Int) => Integer, M : Int, N : Int) = new IntegerMatrix(f,M,N)
  
  override def smithNormalForm = throw new UnsupportedOperationException("not implemented yet")
  override def hermiteNormalForm = throw new UnsupportedOperationException("not implemented yet")
  
}