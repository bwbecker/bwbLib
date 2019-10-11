package ca.bwbecker.utest.tests

import ca.bwbecker.utest.Verify

import utest._


/**
  * A collected sample of failing verifiers.
  */
object SampleFailures extends TestSuite with Verify {

  val tests = Tests {
    "Sample Failures" - {
/*
            "StringVerifier" - {

              "equals" - {verify("A sample string").equals("A SAMPLE string")}
              "contains" - {verify("A sample string").contains("Sample")}
              "startsWith" - {
                verify("A sample string").startsWith("sample string")
              }
              "contains all" - {verify("A sample string").containsAll("A", "SAMPLE", "test")}
            }

            "SeqVerifier" - {
              val lst = List("One", "Two", "Three")
              val vlst = verify(lst)

              "equals" - {
                "missing element" - vlst.equals("Zero" :: lst)
                "extra element" - vlst.equals("One", "Three")
                "wrong order" - vlst.equals("Two", "One", "Three")
              }

              "almost equals" - verify(List("One")).equals(List("Ones"))

              "length" - {
                "length exactly" - vlst.length(4)
                "length at least" - vlst.lengthAtLeast(4)
                "length at most" - vlst.lengthAtMost(2)
              }

              "contains" - {
                "contains item" - vlst.contains("Zero")
                "containsNone" - vlst.containsNone("One")
                "containsOne" - {
                  "none" - vlst.containsOne("Twoo", "Threed")
                  "many" - vlst.containsOne("One", "Three")
                }

                "containsSome" - vlst.containsSome("Twoo", "Threed")
                "containsAll" - vlst.containsAll("Two", "Threed")
              }

              "exists" - vlst.exists(s ⇒ s.endsWith("Z"))

              "forall" - vlst.forall(_.length == 3)

              "setEquals" - {
                "extra in left" - {verify(List(1, 1, 2, 3)).setEquals(List(1, 2))}
                "extra in right" - {verify(List(1, 1, 2)).setEquals(List(1, 2, 2, 2, 3))}
              }

              "with a long list" - {
                "containsSome" - {verify(0 until 50).containsSome(-1, -2)}
              }
            }

            "misc" - {
              "fail" - fail("this is a fail")
            }
*/

    }
  }

}

