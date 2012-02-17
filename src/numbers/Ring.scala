package numbers

/** 
 * A ring, a mathematical object closed under addition
 * subtraction and multiplication. 
 */
trait Ring[R] {
  
  def +(that : R) : R;
  def -(that : R) : R;
  def *(that : R) : R;
  
}

