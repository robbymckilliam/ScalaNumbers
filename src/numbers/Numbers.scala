package numbers

/** Base class for ScalaNumbers */
trait ScalaNumbersObject {
	//nothing to see here for now
}

trait Monoid[M] extends ScalaNumbersObject {
  /** The operation */
  def +(that : M) : M
  /** The identity element*/
  def zero : M
}

/**
 * A group, mathematical object with an operation +, an 
 * identity element zero, and in inverse -
 */
trait Group[G] extends Monoid[G] {
  /** Get the inverse of this group element*/
  def - : G
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
trait Ring[R] extends Group[R] {
  def *(that : R) : R
  
}

/** Ring with multiplicative identity */
trait RingWithUnity[R] extends Ring[R] {
  /** The multiplicative identity */
  def one : R
}

/** 
 * Unique factorisation domain.  A ring such that every element can be
 * expressed as the product of prime factors.
 */
trait UniqueFactorisationDomain[R] extends RingWithUnity[R] {
  /** Norm */
  def factors : Seq[R]
}

/** 
 * Euclidean domain.  A ring with a norm.  The type
 * of the norm can vary.  For example, for the integers
 * the norm is from the natural numbers, for the reals,
 * the norm is a positve real.
 */
trait EuclideanDomain[R,N <: Ordered[N]] extends UniqueFactorisationDomain[R] {
  /** Norm */
  def norm : N
  /** Euclidean division, ie. divide but throw away remainder */
  def / (that : R) : R
  /** Remainder after division */
  def mod(that : R) : R
}

/** Static algorithms for Euclidean domains, mostly related to the Euclidean algorithm */
object EuclideanDomain {
  
  /** Greatest common divisor.
   * Uses recursive algorithm, this should potentially be interative.
   */
  def gcd[R <: EuclideanDomain[R,_]](a : R, b : R) : R = if( b == b.zero ) a else gcd(b, a mod b)
  
  /** Greatest common divisor of two scala Ints*/
  def gcd(a : Int, b : Int) : Int = if( b == 0 ) a.abs else gcd(b.abs, a.abs % b.abs)
  
  /** The Extended Euclidean algorithm applied to two scala Ints*/
  def extended_gcd(a : Int, b : Int) : (Int, Int) = {
    if( b == 0) return (1,0)
    else {
      val q = a/b
      val r = a - b*q
      val (s,t) = extended_gcd(b,r)
      return (t, s - q*t)
    }
  }
  
  /** The Extended Euclidean algorithm.
   * Uses recursive algorithm, this should potentially be interative.
   */
  def extended_gcd[R <: EuclideanDomain[R,_]](a : R, b : R) : (R, R) = {
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
trait Field[F,N <: Ordered[N]] extends EuclideanDomain[F,N]{
  override def /(that : F) : F
  /** The mulitplicative inverse */
  def / : F
  /** mod for a field is always zero */
  override def mod(that : F) : F = this.zero
}

