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
