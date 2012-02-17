package numbers

trait Ring[R] {
  
  def +(that : R) : R;
  def -(that : R) : R;
  def *(that : R) : R;
  
}

