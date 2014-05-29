/*
 * @author Robby McKilliam
 */

package numbers

import org.junit.Test
import org.junit.Assert._

class AlgorithmsTest {
  
   @Test def catchMaxIterationsException = {
    val convitr =  new ConvergentIteration[Double](1.0, x=>x+1.0, (x,y) => false, 20) 
    try{
      val c = convitr.limit
     }catch{
       case e : convitr.MaximumIterationsReachedException => println(e.getMessage + " Limit was " + e.limit)
       case _ : Throwable => fail("exception not correctly caught")
     }
  }
  
}
