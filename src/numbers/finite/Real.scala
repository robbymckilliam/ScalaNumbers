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
  final def toDouble = d
  
  final def - = new Real(-d)
  final def +(that: Real) = new Real(d + that.toDouble)
  final def -(that: Real) = new Real(d - that.toDouble)
  final def +(that: Double) : Real = new Real(d + that)
  final def -(that: Double) : Real = new Real(d - that)
  final def +(that: Int) : Real = new Real(d + that)
  final def -(that: Int) : Real = new Real(d - that)

  final def *(that: Real) = new Real(d * that.toDouble)
  final def /(that: Real) = new Real(d / that.toDouble)
  final def *(that: Double) : Real = new Real(d * that)
  final def /(that: Double) : Real = new Real(d / that)
  final def / : Real = new Real(1.0 / d)
  final def *(that: Int) : Real = new Real(d * that)
  final def /(that: Int) : Real = new Real(d / that)
  
  final def one : Real = Real.one
  final def zero : Real = Real.zero
    
  final def norm : Real = new Real(d.abs)
    
  /// Don't factorise the real numbers!
  final def factors = null
  
  final override def toString : String  = d.toString
}
