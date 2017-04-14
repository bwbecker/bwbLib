package ca.bwbecker

/**
 * Created by bwbecker on 2016-06-12.
 */
package object numeric {


  /**
   * Stuff to test double values to within a given precision
   *
   * Usage:
   * import ca.bwbecker.numeric
   * implicit val precision = Precision(0.0001)
   * assert(1.0000 ~= 1.00005) ==> true
   */
  implicit def add_~=(d:Double) = new WithAlmostEquals(d)

  class WithAlmostEquals(d:Double) {


    def ~=(d2:Double)(implicit p:Precision):Boolean = Math.abs(d-d2) <= p.p
  }


  case class Precision(val p:Double)


}
