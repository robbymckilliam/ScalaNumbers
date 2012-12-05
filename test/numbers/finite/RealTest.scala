/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package numbers.finite

import org.junit.Test;
import org.junit.Assert._;

class RealTest {
  
  @Test def operateWithDoubleTest() {
    val tol = 1e-7
    assertTrue(Real(1.0).norm==1.0)
    assertTrue( (Real(1.0) * 20.0 - 20.0) < Real(tol) )
    assertTrue( (Real(1.0) * 20.0 - 20.0) < tol )
    assertTrue( (Real(1.0) * 20.0 - 20.0) < 1 )
    
  }
  
  

}
