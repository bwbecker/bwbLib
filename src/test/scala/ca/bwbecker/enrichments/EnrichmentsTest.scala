package ca.bwbecker.enrichments

import utest.TestSuite
import utest._

import java.time.LocalTime
import scala.concurrent.duration._

/**
  * Created by bwbecker on 2016-06-17.
  */
object EnrichmentsTest extends TestSuite {

  val tests = this {


    "RichRegEx" - {
      val re = "ab*c".r

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

        "Ternary operator" - {
          (true ? "yes" | "no") ==> "yes"
          (false ? "yes" | "no") ==> "no"
        }
      }
    }


    "RichLocalTime" - {
      val t = LocalTime.of(10, 11, 12, 0)

      "Add a duration" - {
        (t + 1.minutes) ==> LocalTime.of(10, 12, 12, 0)
        (t + 1.second) ==> LocalTime.of(10, 11, 13, 0)
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

      "less" - {
        "remove from Vector" - {
          val v = Vector("one", "two", "three", "four", "three")

          "remove only element" - {
            v.less("two") ==> Vector("one", "three", "four", "three")
          }

          "remove only one when dups" - {
            v.less("three") ==> Vector("one", "two", "four", "three")
          }

          "don't crash if not exists" - {
            v.less("five") ==> Vector("one", "two", "three", "four", "three")
          }

          "works with something other than strings" - {
            val v2 = Vector((1,1), (2,2), (3,3), (4,4), (3,3))
            v2.less((3,3)) ==> Vector((1,1), (2,2), (4,4), (3,3))
          }
        }


        "remove from List" - {
          val lst = List("one", "two", "three", "four", "three")

          "remove only element" - {
            lst.less("two") ==> List("one", "three", "four", "three")
          }

          "remove only one when dups" - {
            lst.less("three") ==> List("one", "two", "four", "three")
          }

          "don't crash if not exists" - {
            lst.less("five") ==> List("one", "two", "three", "four", "three")
          }

          "works with something other than strings" - {
            val lst2 = List((1,1), (2,2), (3,3), (4,4), (3,3))
            lst2.less((3,3)) ==> List((1,1), (2,2), (4,4), (3,3))
          }
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


    "RichString" - {
      "convert a non-empty string to an option string" - {
        "a string".toOption ==> Some("a string")
      }

      "convert an empty string to a None" - {
        "".toOption ==> None
      }

      "convert a null string to a None" - {
        val s: String = null
        s.toOption ==> None
      }

      "return a short capped string as-is" - {
        "abc".cap(3) ==> "abc"
      }

      "cap a string" - {
        "abcd".cap(3) ==> "ab…"
      }
    }


    "RichVector" - {
      val v1 = Vector(0, 1, 2, 3)

      "provide a withoutIndex method that" - {
        "works with Vectors" - {

          v1.withoutIndex(0) ==> Vector(1, 2, 3)
          v1.withoutIndex(1) ==> Vector(0, 2, 3)
          v1.withoutIndex(3) ==> Vector(0, 1, 2)

          v1.withoutIndex(-1) ==> v1
        }

        "returns an collection of the same type" - {
          v1.withoutIndex(0).getClass.getName ==> "scala.collection.immutable.Vector"
        }
      }

      "provide a without method that" - {
        "works with Vectors" - {
          val v = Vector("zero", "one", "two", "three")

          v.without("zero") ==> Vector("one", "two", "three")
          v.without("one") ==> Vector("zero", "two", "three")
          v.without("three")  ==> Vector("zero", "one", "two")

          v.without("four")  ==> v

          Vector("one", "two").without("two") ==> Vector("one")
        }

      }

    }


  }
}
