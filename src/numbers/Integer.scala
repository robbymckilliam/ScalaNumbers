/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package numbers

import bignums.BigInteger

object Integer {
  
}

abstract class AbstractInteger extends EuclideanDomain[Integer, Integer]{
  /** 
   *Internally use a bignums.BigInteger.  This is much faster than java.BigInteger (and correspondingly)
   *scalas BigInt for very larger integers.  If java.BigInteger ever gets fixed then this could be changed.
   */
  val bigint : BigInteger 
  
}

