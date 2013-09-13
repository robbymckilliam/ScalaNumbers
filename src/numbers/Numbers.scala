package numbers

import scala.annotation.tailrec

trait Monoid[M <: Monoid[M]] {
  /** The operation */
  def +(that : M) : M
  /** The identity element*/
  def zero : M
  /** Test for equality */
  def ==(that : M) : Boolean
  def !=(that : M) = !(this==that)
}

/**
 * A group, mathematical object with an operation +, an 
 * identity element zero, and in inverse -
 */
trait Group[G <: Group[G]] extends Monoid[G] {
  /** Get the inverse of this group element.  Scala unary_ allow - to be a prefix */
  def unary_- : G
  /** Operate with the inverse */
  def -(that : G) : G
  /** Test for equality */
  //def ==(that : G) : Boolean
  /** Test for equality */
  //def !=(that : G) : Boolean
}

/** 
 * A ring, a mathematical object closed under addition
 * subtraction and multiplication.  Also has an 
 * additive identity.
 */
trait Ring[R <: Ring[R]] extends Group[R] {
  def *(that : R) : R
  
}

/** Ring with multiplicative identity */
trait RingWithUnity[R <: RingWithUnity[R]] extends Ring[R] {
  /** The multiplicative identity */
  def one : R
}

/** 
 * Unique factorisation domain.  A ring such that every element can be
 * expressed as the product of prime factors.
 */
trait UniqueFactorisationDomain[R <: UniqueFactorisationDomain[R]] extends RingWithUnity[R] {
  /** Sequence of factors */
  def factors : Seq[R]
}

/** 
 * Euclidean domain.  A ring with a norm.  The type
 * of the norm can vary.  For example, for the integers
 * the norm is from the natural numbers, for the reals,
 * the norm is a positve real.
 */
trait EuclideanDomain[R <: EuclideanDomain[R,N],N <: Ordered[N]] extends UniqueFactorisationDomain[R] {
  /** Norm */
  def norm : N
  /** Euclidean division, ie. divide but throw away remainder */
  def / (that : R) : R
  /** Remainder after division */
  def mod(that : R) : R
}

/** Static algorithms for Euclidean domains, mostly related to the Euclidean algorithm */
object EuclideanDomain {
  
  /** Greatest common divisor.*/
  @tailrec final def gcd[R <: EuclideanDomain[R,_]](a : R, b : R) : R = if( b == b.zero ) a else gcd(b, a mod b)
  /** Greatest common divisor. gcd for Integer is always positive. */
  @tailrec final def gcd(a : Integer, b : Integer) : Integer = if( b == Integer.zero ) a.abs else gcd(b.abs, a.abs mod b.abs)
  /** Greatest common divisor of two scala Ints*/
  @tailrec final def gcd(a : Int, b : Int) : Int = if( b == 0 ) a.abs else gcd(b.abs, a.abs % b.abs)
  /** Greatest common divisor of two scala Longs*/
  @tailrec final def gcd(a : Long, b : Long) : Long = if( b == 0 ) a.abs else gcd(b.abs, a.abs % b.abs)
  
  /** The Extended Euclidean algorithm applied to two scala Ints*/
  final def extended_gcd(a : Int, b : Int) : (Int, Int) = {
    if( b == 0) return (1,0)
    else {
      val q = a/b
      val r = a - b*q
      val (s,t) = extended_gcd(b,r)
      return (t, s - q*t)
    }
  }
  
  /** The Extended Euclidean algorithm applied to two scala Ints*/
  final def extended_gcd(a : Long, b : Long) : (Long, Long) = {
    if( b == 0) return (1,0)
    else {
      val q = a/b
      val r = a - b*q
      val (s,t) = extended_gcd(b,r)
      return (t, s - q*t)
    }
  }
  
  /** The Extended Euclidean algorithm.
   * Uses recursive algorithm, this should potentially be interative or tail recursive 
   */
  final def extended_gcd[R <: EuclideanDomain[R,_]](a : R, b : R) : (R, R) = {
    if( b == b.zero ) return (b.one,b.zero)
    else {
      val q = a/b
      val r = a - b*q
      val (s,t) = extended_gcd(b,r)
      return (t, s - q*t)
    }
  }
  
}

/** 
 * A field, a mathematical object closed under addition
 * subtraction and multiplication and division. 
 */
trait Field[F <: Field[F,N],N <: Ordered[N]] extends EuclideanDomain[F,N] {
  override def /(that : F) : F
  /** mod for a field is always zero */
  override def mod(that : F) : F = this.zero
  /** Sequence of factors */
  override def factors : Seq[F] = List(this.one,this.asInstanceOf[F])
}

