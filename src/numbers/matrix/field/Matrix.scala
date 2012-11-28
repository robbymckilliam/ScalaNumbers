/*
 * @author Robby McKilliam
 */

package numbers.matrix.field

import numbers.Field

class Matrix[F <: Field[F,_]](val f : (Int, Int) => F, val M : Int, val N : Int) {

  def apply(m : Int, n : Int) = {
    if(m >= M || m < 0 || n >= N || n < 0) throw new ArrayIndexOutOfBoundsException("You attempted to access an element out of range")
    f(m,n)
  }
  
  def backwitharray = {
    val A = new scala.collection.mutable.ArraySeq[F](N*M)
    for( m <- 0 until M; n <- 0 until N ) A(m*N + n) = f(m,n)
    new Matrix[F]( (m,n) => A(m*N+n), M, N )
  }
  
  def +(that: Matrix[F]) = new Matrix[F]( (m,n) => this(m,n) + that(m,n), M, N )
  def -(that: Matrix[F]) = new Matrix[F]( (m,n) => this(m,n) - that(m,n), M, N)
  def *(that: Matrix[F]) = new Matrix[F]( (m,n) => (0 until N).foldLeft(f(0,0).zero)( (v, i) => v + this(m,i)*that(i,n)), M, that.N ).backwitharray
  
}
