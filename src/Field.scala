package numbers

trait Field[F] extends Ring[F]{
  def /(that : F) : F;
}