package numbers

import scala.annotation.tailrec

trait Element[E <: Element[E]] {
  /** Test for equality */
  def ==(that : E) : Boolean
  /** Test for inequality */
  def !=(that : E) = !(this == that)
  /** Compatibility with Scala's equals method */
  final override def equals(that : Any) = throw new UnsupportedOperationException("Scala's equals method is not used in the numbers library.  Override =='s instead.  I'm aware that this is frowned upon!")
}

trait Monoid[M <: Monoid[M]] extends Element[M] {
  /** The operation */
  def +(that : M) : M
  /** The identity element*/
  def zero : M
}

/**
 * A group, mathematical object with an operation +, an 
 * identity element zero, and in inverse -
 */
trait Group[G <: Group[G]] extends Monoid[G] {
  /** Get the inverse of this group element.  Scala unary_ allow - to be a prefix */
  def unary_- : G
  /** Operate with the inverse */
  def -(that : G) : G = this + -that
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

object RingWithUnity {
  /// this*this*this*...*this k times.  Exponentiation by squaring
  def pow[R <: RingWithUnity[R]](base : R, k : numbers.Integer) : R = {
    if(k<0) throw new RuntimeException("Exponent k must be nonnegative.")
    @tailrec def f(result: R, v : R, i : numbers.Integer) : R = {
      if(i==Integer.zero) return result;
      else if(i.mod(2)==Integer.one) return f(result*v,v*v,i/2)
      else return f(result, v*v, i/2)
    }
    return f(base.one,base,k)
  }
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
  /** Returns true if the norm of this is larger than the norm of that */
  def normlarger(that : R) : Boolean = this.norm > that.norm
}

/** Static algorithms for Euclidean domains, mostly related to the Euclidean algorithm */
object EuclideanDomain {
  
  /** Greatest common divisor.*/
  @tailrec final def gcd[R <: EuclideanDomain[R,_]](a : R, b : R) : R = if( b == b.zero ) a else gcd(b, a mod b)
  final def gcd[R <: EuclideanDomain[R,_]](s : Seq[R]) : R = s.reduceLeft { (g, v) => gcd[R](g,v) }
  
  /** Greatest common divisor. gcd for Integer is always positive. */
  @tailrec final def gcd(a : Integer, b : Integer) : Integer = if( b == Integer.zero ) a.abs else gcd(b.abs, a.abs mod b.abs)
  final def gcd(s : Seq[Integer]) : Integer = s.reduceLeft { (g,v) => gcd(g,v) }
  /** Least common multiple of Integers */
  final def lcm(a : Integer, b : Integer) : Integer = (a*b).abs / gcd(a,b)
  final def lcm(s : Seq[Integer]) : Integer = s.reduceLeft { (g,v) => lcm(g,v) }
  
  /** Greatest common divisor of two scala Ints*/
  @tailrec final def gcd(a : Int, b : Int) : Int = if( b == 0 ) a.abs else gcd(b.abs, a.abs % b.abs)
  final def gcd(s : Seq[Int]) : Int = s.reduceLeft { (g,v) => gcd(g,v) }
  /** Least common multiple of scala Int */
  final def lcm(a : Int, b : Int) : Int = (a*b).abs / gcd(a,b)
  final def lcm(s : Seq[Int]) : Int = s.reduceLeft { (g,v) => lcm(g,v) }
  
  /** Greatest common divisor of two scala Longs*/
  @tailrec final def gcd(a : Long, b : Long) : Long = if( b == 0 ) a.abs else gcd(b.abs, a.abs % b.abs)
  final def gcd(s : Seq[Long]) : Long = s.reduceLeft { (g,v) => gcd(g,v) }
  /** Least common multiple of scala Long */
  final def lcm(a : Long, b : Long) : Long = (a*b).abs / gcd(a,b)
  final def lcm(s : Seq[Long]) : Long = s.reduceLeft { (g,v) => lcm(g,v) }
  
  /** The Extended Euclidean algorithm applied to two scala Ints. */
  final def extended_gcd(a : Int, b : Int) : (Int, Int) = extended_gcd_rec(a,b,1,0,0,1)
  @tailrec final private def extended_gcd_rec(r0 : Int, r1 : Int, s0 : Int, s1 : Int, t0 : Int, t1 : Int) : (Int, Int) = {
    if(r1 == 0) return (s0, t0)
    else {
      val q = r0/r1
      return extended_gcd_rec(r1, r0-r1*q, s1, s0-s1*q, t1, t0-t1*q)
    }
  }
  
  /** The Extended Euclidean algorithm applied to two scala Longs. */
  final def extended_gcd(a : Long, b : Long) : (Long, Long) = extended_gcd_rec(a,b,1,0,0,1)
  @tailrec final private def extended_gcd_rec(r0 : Long, r1 : Long, s0 : Long, s1 : Long, t0 : Long, t1 : Long) : (Long, Long) = {
    if(r1 == 0) return (s0, t0)
    else {
      val q = r0/r1
      return extended_gcd_rec(r1, r0-r1*q, s1, s0-s1*q, t1, t0-t1*q)
    }
  }
  
  /** The Extended Euclidean algorithm. Tail recursive. */
  final def extended_gcd[R <: EuclideanDomain[R,_]](a : R, b : R) : (R, R) = extended_gcd_rec[R](a,b,a.one,a.zero,a.zero,a.one)
  @tailrec final private def extended_gcd_rec[R <: EuclideanDomain[R,_]](r0 : R, r1 : R, s0 : R, s1 : R, t0 : R, t1 : R) : (R, R) = {
    if(r1 == r0.zero) return (s0, t0)
    else {
      val q = r0/r1
      return extended_gcd_rec(r1, r0-r1*q, s1, s0-s1*q, t1, t0-t1*q)
    }
  }
  
  /** The extended Euclidean algorithm applied to more than 2 numbers */
  final def extended_gcd[R <: EuclideanDomain[R,_]](a : Seq[R]) : List[R] = {
    val (s, t) = extended_gcd[R](a.head, gcd(a.tail))
    if(a.size == 2) return List(s,t)
    return s :: (extended_gcd[R](a.tail).map(x => x*t))
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
  /** Multiplicative inverse */
  def reciprocal: F
  /** Sequence of factors */
  override def factors : Seq[F] = List(this.one,this.asInstanceOf[F])
}

