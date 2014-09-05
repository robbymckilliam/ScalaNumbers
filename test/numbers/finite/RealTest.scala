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
  
  @Test def doubleImplicits() {
    val tol = 1e-7
    assertTrue( 1.0 + Real.zero == Real.one )
    assertTrue( Real.zero + 1.0 == Real.one )
    assertTrue( 1.0 + Real.zero == 1 )
    assertTrue( Real.zero + 1.0 == 1.0 )
    assertTrue( (Real.one * 2.0 - Real(2.0)).norm < tol )
    assertTrue( (2.0*Real.one - Real(2.0)).norm < tol )
    assertTrue( (2.0 - 2.0*Real.one).norm < tol )
    assertTrue( ((2.0 - 3.0*Real.one) + 1.0).norm < tol )
    assertTrue( 1.0 == Real.one )
    assertTrue( 0.0 == Real.zero)
    assertTrue( Real.one == 1.0 )
    assertTrue( Real.zero == 0.0)
  }
  
   @Test def IntImplicits() {
    val tol = 1e-7
    assertTrue( 1 + Real.zero == Real.one )
    assertTrue( Real.zero + 1 == Real(1) )
    assertTrue( Real.zero + 1 == 1.0 )
    assertTrue( 1 + Real.zero == 1 )
    assertTrue( (Real.one * 2 - Real(2)).norm < tol )
    assertTrue( (2*Real.one - Real(2)).norm < tol )
    assertTrue( (2 - 2*Real.one).norm < tol )
    assertTrue( (2 - 3*Real.one + 1).norm < tol )
    assertTrue( (2 - 3*Real.one + Real(1)).norm < tol )
    assertTrue( 1 == Real.one )
    assertTrue( 0 == Real.zero)
    assertTrue( Real.one == 1 )
    assertTrue( Real.zero == 0 )
  }
  
  

}
