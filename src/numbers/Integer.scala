/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package numbers

import bignums.BigInteger

object Integer {
  val one = new IntegerFromInt(1)
  val zero = new IntegerFromInt(0)
}

abstract class Integer extends EuclideanDomain[Integer, Integer] with Ordered[Integer] {
  /** 
   *Internally use a bignums.BigInteger.  This is much faster than java.BigInteger (and correspondingly)
   *scalas BigInt for very larger integers.  If java.BigInteger ever gets fixed then this could be changed.
   */
  val bigint : BigInteger 
  
  def +(that : Integer) : Integer = new IntegerFrombignumsBigInteger(bigint.add(that.bigint))
  def - : Integer = new IntegerFrombignumsBigInteger(bigint.negate)
  def -(that : Integer) : Integer = new IntegerFrombignumsBigInteger(bigint.subtract(that.bigint))
  def *(that : Integer) : Integer = new IntegerFrombignumsBigInteger(bigint.multiply(that.bigint))
  
  /** Not yet implemented */
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

class IntegerFromInt(val x : Int) extends Integer {
  override val bigint : BigInteger = new BigInteger(x.toString)
}

class IntegerFromLong(val x : Long) extends Integer {
  override val bigint : BigInteger = new BigInteger(x.toString)
}

class IntegerFromBigInt(val x : BigInt) extends Integer {
  override val bigint : BigInteger = new BigInteger(x.toString)
}

class IntegerFrombignumsBigInteger(val x : BigInteger) extends Integer {
  override val bigint = x
}
