/*
 * @author Robby McKilliam
 */

package numbers

import org.junit.Test
import org.junit.Assert._
import numbers.Algorithms.ConvergentIteration
import numbers.Algorithms.MaximumIterationsReachedException
import numbers.Algorithms.GeneralisedContinuedFraction
import numbers.Algorithms.InfiniteGeneralisedContinuedFraction

class AlgorithmsTest {
  
   @Test def catchMaxIterationsException = {
    val convitr =  new ConvergentIteration[Double](1.0, x=>x+1.0, (x,y) => false, 20) 
    try{
      val c = convitr.limit
     }catch{
       case e : MaximumIterationsReachedException[Double] => println(e.getMessage + " Limit was " + e.limit)
       case _ : Throwable => fail("exception not correctly caught")
     }
  }
  
  @Test def generalisedContinuedFractionTest = {
    { //finite continued fraction for 415/93
      val r = Rational(415,93)
      val b = List[Rational](4,2,6,7)
      val a = List[Rational](1,1,1,1)
      val rc = new GeneralisedContinuedFraction[Rational](a,b,3).value
      assertTrue(r == rc)
    }
    { //continued fraction for ln2
      def b(n: Int) = Rational(n,1)
      def a(n : Int) = if(n==1) Rational.one else Rational((n/2)*(n/2),1) //floor being computed by integer division
      val rc = new GeneralisedContinuedFraction[Rational](a,b,20).value
      val r = scala.math.log(2)
      assertTrue( (r - rc.toDouble).abs < 1e-10 )
    }
    { //continued fraction for ln2
      val tol = Rational(1,10000)
      def b(n: Int) = Rational(n,1)
      def a(n : Int) = if(n==1) Rational.one else Rational((n/2)*(n/2),1) //floor being computed by integer division
      val rc = new InfiniteGeneralisedContinuedFraction[Rational](a,b,tol).value
      val r = scala.math.log(2)
      assertTrue( tol.toDouble > (r - rc.toDouble).abs )
    }
    { //continued fraction for ln2
      val tol = Rational(1,10000000)
      def b(n: Int) = Rational(n,1)
      def a(n : Int) = if(n==1) Rational.one else Rational((n/2)*(n/2),1) //floor being computed by integer division
      val rc = new InfiniteGeneralisedContinuedFraction[Rational](a,b,tol).value
      val r = scala.math.log(2)
      assertTrue( tol.toDouble > (r - rc.toDouble).abs )
    }
    
  }
  
}
