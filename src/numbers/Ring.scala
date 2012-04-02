package numbers

/** Base class for ScalaNumbers */
trait ScalaNumbersObject {
	//nothing to see here for now
}

/**
 * A group, mathematical object with an operation +, an 
 * identity element zero, and in inverse -
 */
trait Group[G] extends ScalaNumbersObject {
  /** The group operation */
  def +(that : G) : G
  /** The additive identity */
  def zero : G
  /** Get the inverse of this group element*/
  def - : G
  /** Operate with the inverse */
  def -(that : G) : G
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
 * Euclidean domain.  A ring with a norm.  The type
 * of the norm can vary.  For example, for the integers
 * the norm is from the natural numbers, for the reals,
 * the norm is a positve real.
 */
trait EuclideanDomain[R,N] extends RingWithUnity[R] {
  /** Norm */
  def norm : N
}

/** 
 * A field, a mathematical object closed under addition
 * subtraction and multiplication and division. 
 */
trait Field[F,N] extends EuclideanDomain[F,N]{
  def /(that : F) : F
  /** The mulitplicative inverse */
  def / : F
}

