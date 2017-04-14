package ca.bwbecker.enrichments

import utest.TestSuite
import utest._

/**
  * Created by bwbecker on 2016-06-17.
  */
object EnrichmentsTest extends TestSuite {

  val tests = this {

    val re = "ab*c".r

    "RichRegEx" - {
      "ab*c matches" - {
        "ac" - {
          re.matches("ac") ==> true
        }

        "abc" - {
          re.matches("abc") ==> true
        }

        "abbbbc" - {
          re.matches("abbbbc") ==> true
        }
      }

      "ab*c misses" - {
        "ab" - {
          re.misses("ab") ==> true
        }
        "bbbc" - {
          re.misses("bbbc") ==> true
        }
        "<empty string>" - {
          re.misses("") ==> true
        }
      }
    }


    "RichBoolean" - {
      "true.option" - {

        "Some" - {
          (1 == 1).option("TRUE") ==> Some("TRUE")
        }

        "None" - {
          (1 == 2).option("TRUE") ==> None
        }
      }
    }

    "RichTraversable" - {
      "sumBy" - {
        "integers in a vector" - {
          val v = Vector(("a", 1), ("b", 2), ("c", 3), ("d", 4))
          v.sumBy(_._2) ==> 10
        }

        "doubles in a list" - {
          val l = List(("a", 1.0), ("b", 2.0), ("c", 3.0), ("d", 4.0))
          l.sumBy(x ⇒ x._2) ==> 10.0
        }
      }
    }


    "RichDouble" - {
      implicit val precision = Precision(0.0001)

      "Almost equals within 0.0001" - {
        (1.0000 ~= 1.00001) ==> true
      }

      "Almost equals NOT within 0.0001" - {
        (1.0000 ~= 1.00011) ==> false
      }
    }

  }
}
