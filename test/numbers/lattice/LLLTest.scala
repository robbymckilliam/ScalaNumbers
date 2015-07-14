package numbers.lattice

import numbers.Rational
import numbers.RationalMatrix
import numbers.finite.RealMatrix
import org.junit._
import Assert._

class LLLTest {

  @Test
  def RationalIdentityIsLLLReduced = {
    val n = 3
    val M = RationalMatrix.identity(n)
    val lll = new RationalLLL(M).reducedBasis
    println(lll)
    assertTrue( lll == M )
  }
  
  @Test
  def RealIdentityIsLLLReduced = {
    val n = 3
    val M = RealMatrix.identity(n)
    val lll = new RealLLL(M).reducedBasis
    println(lll)
    assertTrue( lll == M )
  }
  
//  @Test
//  def detectsNotBasis = {
//    val n = 3
//    val b = Array(
//      Array(1,1,2),
//      Array(1,2,3),
//      Array(1,3,4)
//    )
//    val M = RationalMatrix( (m,n) => Rational(b(m)(n)), n, n )
//    try { val lll = new RationalLLL(M).reducedBasis } catch {
//      case e : RuntimeException => println("Not full rank exception caught correctly")
//      case _ : Throwable => fail("exception not correctly caught")
//    }
//  }

}
