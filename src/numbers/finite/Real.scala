/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package numbers.finite

import numbers.Field

/**
 * Provides static definitions of the multiplicative 
 * and additive identities
 */
object Real {
  val one = new Real(1.0) 
  val zero = new Real(0.0)
}

class Real(d : Double) extends Field[Real, Real] {
  def toDouble = d
  
  def +(that: Real) = new Real(d + that.toDouble)
  def -(that: Real) = new Real(d - that.toDouble)
  def +(that: Double) : Real = new Real(d + that)
  def -(that: Double) : Real = new Real(d - that)

  def *(that: Real) = new Real(d * that.toDouble)
  def /(that: Real) = new Real(d / that.toDouble)
  def *(that: Double) : Real = new Real(d * that)
  def /(that: Double) : Real = new Real(d / that)
  
  def one : Real = Real.one
  def zero : Real = Real.zero
    
  def norm : Real = new Real(d.abs)
    
  override def toString : String  = d.toString
}
