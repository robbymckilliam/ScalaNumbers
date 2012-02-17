package numbers

/** 
 * A field, a mathematical object closed under addition
 * subtraction and multiplication and division. 
 */
trait Field[F] extends Ring[F]{
  def /(that : F) : F;
}