package numbers

/** Base class for ScalaNumbers */
trait ScalaNumbersObject {
	//nothing to see here for now
}

/** 
 * A ring, a mathematical object closed under addition
 * subtraction and multiplication.  Also has an 
 * additive identity.
 */
trait Ring[R] extends ScalaNumbersObject {
  
  def +(that : R) : R
  def -(that : R) : R
  def *(that : R) : R
  
  /** The additive identity */
  def zero : R
  
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
}

