/*
 * @author Robby McKilliam
 */

package numbers

import numbers.EuclideanDomain.gcd
import numbers.matrix.MatrixWithElementsFromAField
import scala.language.implicitConversions

object Rational {
  val one = Rational(1,1)
  val zero =  Rational(0,1)
  
  def apply(n : Integer, d : Integer) = new Rational(n,d)
  def apply(n : Int, d : Int) : Rational = Rational(Integer(n),Integer(d))
  def apply(n : Long, d : Long) : Rational = Rational(Integer(n),Integer(d))
  implicit def toRational(i : Integer) = Rational(i,1)
  implicit def toRational(i : Long) = Rational(i,1)
  implicit def toRational(i : Int) = Rational(i,1)
}

/** Infinite precision rational number.  Will grow until your computer runs out of memory. */
class Rational(protected val num : Integer, protected val den: Integer) extends Field[Rational,Rational] with Ordered[Rational] {
  
  //find relatively prime simple fraction n/d = num/den
  val (n, d) = {    
    if(den==Integer.zero) throw new RuntimeException("Denominator in rational is zero!")
    val g = gcd(num,den)
    if(den < Integer.zero) (-num/g,-den/g) //denominator is always positive
    else (num/g,den/g)
  }
  
  final def numerator = n
  final def denominator = d
  final def zero = Rational.zero
  final def one = Rational.one
  
  final def +(that : Rational) : Rational = Rational(that.d*n + that.n*d,that.d*d)
  final def *(that : Rational) : Rational = Rational(that.n*n,that.d*d)
  final def /(that: Rational) : Rational = Rational(that.d*n,that.n*d)
  
  final def +(that : Integer) : Rational = Rational(that*d + n,d)
  final def -(that : Integer) : Rational = this + (-that)
  final def *(that : Integer) : Rational = Rational(that*n,d)
  final def /(that: Integer) : Rational = Rational(n,that*d)
  
  final def unary_- : Rational = Rational(-n,d) //n- is the negative of n
  
  final def norm : Rational = Rational(n.norm,d) //same as absolute value
  
  final override def ==(that : Rational) : Boolean = that.n==n && that.d==d
  
  ///Return true if this rational number is a whole integer (i.e. the denominator is zero)
  final def isInteger = d == Integer.one
  
  /** Uses scala's internal Ordered, only need to override compare */
  final def compare(that : Rational) : Int = {
    val num = (this-that).n
    if(num < Integer.zero) return -1
    else if(num > Integer.zero) return 1
    else return 0
  }
  
  final override def toString : String  = n.toString + "/" + d.toString
  
}

object RationalMatrix {
    /// contruct identity matrix
  def identity(M : Int, N : Int) : RationalMatrix = new RationalMatrix( (m,n) => if(m==n) Rational.one else Rational.zero, M,N)
  def identity(N : Int): RationalMatrix = identity(N,N)
  def apply(f : (Int,Int) => Rational, M : Int, N : Int) = construct(f,M,N)
  def apply(a : Seq[Seq[Rational]]) : RationalMatrix = new RationalMatrix((m,n) => a(m)(n),a.length,a(0).length) 
  def asRow(r : Seq[Rational]) = new RationalMatrix( (m,n) => r(n), 1, r.length)
  def asColumn(r : Seq[Rational]) = new RationalMatrix( (m,n) => r(m), r.length, 1)
  def construct(f : (Int,Int) => Rational, M : Int, N : Int) = new RationalMatrix(f,M,N)
  def constructRow(f : (Int) => Rational, N : Int) = construct( (m,n) => f(n), 1, N)
  def constructColumn(f : (Int) => Rational, M : Int) = construct( (m,n) => f(m), M, 1)
}

/** Matrix with rational elements */
class RationalMatrix(f : (Int,Int) => Rational, override val M : Int, override val N : Int) 
  extends MatrixWithElementsFromAField[Rational, RationalMatrix] {

  override protected def get(m : Int, n : Int) = f(m,n)
  override def construct(f : (Int,Int) => Rational, M : Int, N : Int) = RationalMatrix(f,M,N)
  
  override def smithNormalForm = throw new UnsupportedOperationException("not implemented yet")
  override def hermiteNormalForm = throw new UnsupportedOperationException("not implemented yet")
  
  /** Sum of the squared magnitudes of all of the elements */
  lazy val squaredFrobeniusNorm : Rational = indices.foldLeft(Rational.zero)( (v, i) => v + this(i)*this(i))
  
}