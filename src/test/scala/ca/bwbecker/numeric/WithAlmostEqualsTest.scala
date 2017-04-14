package ca.bwbecker.numeric


import utest.TestSuite
import utest._

/**
 * Created by bwbecker on 2016-05-31.
 */
object WithAlmostEqualsTest extends TestSuite {

  val tests = this {
    implicit val precision = Precision(0.0001)

    "Almost equals within 0.0001" - {
      (1.0000 ~= 1.00001) ==> true
    }

    "Almost equals NOT within 0.0001" - {
      (1.0000 ~= 1.00011) ==> false
    }
  }
}