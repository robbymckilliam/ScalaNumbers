package numbers

/** 
 * A ring, a mathematical object closed under addition
 * subtraction and multiplication.  Also has additive
 * and multiplicative identities.
 */
trait Ring[R] extends ScalaNumbersObject {
  
  def +(that : R) : R;
  def -(that : R) : R;
  def *(that : R) : R;
  
  /** The additive identity */
  def zero : R
  
  /** The multiplicative identity */
  def one : R
  
}

