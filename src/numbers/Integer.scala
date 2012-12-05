/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
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
class Integer(val bigint : BigInteger) extends EuclideanDomain[Integer, Integer] with Ordered[Integer] {
  
  def +(that : Integer) : Integer = new IntegerFrombignumsBigInteger(bigint.add(that.bigint))
  def +(that : Int) : Integer = this + new IntegerFromInt(that)
  def - : Integer = new IntegerFrombignumsBigInteger(bigint.negate)
  def -(that : Integer) : Integer = new IntegerFrombignumsBigInteger(bigint.subtract(that.bigint))
  def -(that : Int) : Integer = this - new IntegerFromInt(that)
  def *(that : Integer) : Integer = new IntegerFrombignumsBigInteger(bigint.multiply(that.bigint))
  def *(that : Int) : Integer = this * new IntegerFromInt(that)
  
  /** Not implemented */
  def factors : Seq[Integer] = throw new UnsupportedOperationException("Not implemented")
  
  def norm : Integer = new IntegerFrombignumsBigInteger(bigint.abs)
  def / (that : Integer) : Integer = new IntegerFrombignumsBigInteger(bigint.divide(that.bigint))
  def mod (that : Integer) : Integer = new IntegerFrombignumsBigInteger(bigint.mod(that.bigint)) 
  
  final def zero : Integer = Integer.zero
  final def one : Integer = Integer.one
  
  def ==(that : Integer) : Boolean = bigint.equals(that.bigint)
  def !=(that : Integer) : Boolean = !bigint.equals(that.bigint)
  
  final def compare(that : Integer) : Int = bigint.compareTo(that.bigint)
  
  override def toString : String = bigint.toString
  
}

class IntegerFromInt(val x : Int) extends Integer(new BigInteger(x.toString))
class IntegerFromLong(val x : Long) extends Integer(new BigInteger(x.toString))
class IntegerFromBigInt(val x : BigInt) extends Integer(new BigInteger(x.toString))
class IntegerFrombignumsBigInteger(val x : BigInteger) extends Integer(new BigInteger(x.toString))

/** A matrix containing integers */
class IntegerMatrix( val f : (Int,Int) => Integer, override val M : Int, override val N : Int)  
  extends MatrixWithElementsFromAEuclideanDomain[Integer, IntegerMatrix] {
    
  override protected def get(m : Int, n : Int) = f(m,n)
  override def construct(f : (Int,Int) => Integer, M : Int, N : Int) = new IntegerMatrix(f,M,N)
  
  override def smithNormalForm = throw new UnsupportedOperationException("not implemented yet")
  override def hermiteNormalForm = throw new UnsupportedOperationException("not implemented yet")
  
}