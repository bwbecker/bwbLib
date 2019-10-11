package ca.bwbecker.utest.tests

import ca.bwbecker.utest.{VerificationError, Verify}

import utest._
import utest.ufansi.Str

object SeqVerifierTests extends TestSuite with Verify {

  /**
    * Returns a plaintext version of the intercepted Verification Error message.
    * @param body Produces a Verification Error.
    */
  private def interceptVerificationError(body: ⇒ Unit): String = {
    val fansi_msg = intercept[VerificationError]{ body }.getMessage
    println(fansi_msg) // uncomment for more sample failures
    Str(fansi_msg).plainText
  }

  val tests = Tests {

    val s = Seq("one", "two", "three")

    "equals" - {
      "equal" - {
        verify(s).equals(s).length(3).u
      }

      "left has extra" - {
        val msg = interceptVerificationError { verify("zero" +: s).equals(s) }
        assert(msg.contains("contained extraneous zero"))
      }

      "right has extra" - {
        val msg = interceptVerificationError {verify(s).equals("zero" +: s)}
        assert(msg.contains("does not contain zero"))
      }

      "not equal based on order" - {
        val msg = interceptVerificationError {verify(s).equals(s.reverse)}
        assert(msg.contains("one"))
        assert(msg.contains("differs from"))
        assert(msg.contains("three"))
      }

      "does not allow usage of Scala's object equality" - {
        val e = intercept[NotImplementedError] { verify(s).equals( verify(s) ) }
        e.getMessage ==> "an implementation is missing"

        val e2 = intercept[NotImplementedError] { verify(s) == verify(s) }
        e2.getMessage ==> "an implementation is missing"
      }
    }

    "contains" - {
      "does contain" - {
        verify(s).contains("one").u
      }

      "does not contain" - {
        val msg = interceptVerificationError (verify(s).contains("zero"))
        assert(msg.contains("does not contain"))
        assert(msg.contains("zero"))
      }
    }

    "containsOne" - {
      "does contain one" - {
        verify(s).containsOne("one", "five", "nine").u
      }

      "does not contain any" - {
        val msg = interceptVerificationError { verify(s).containsOne("zero", "five") }
        assert(msg.contains("contains none of"))
        assert(msg.contains("zero"))
        assert(msg.contains("five"))
      }

      "contains several" - {
        val msg = interceptVerificationError { verify(s).containsOne("one", "two") }
        assert(msg.contains("contains"))
        assert(msg.contains("one, two"))
      }
    }

    "containsAll" - {
      "does contain all" - {
        verify(s).containsAll("one", "two").u
      }

      "does not contain some" - {
        val msg = interceptVerificationError { verify(s).containsAll("zero", "one", "two") }
        assert(msg.contains("does not contain"))
        assert(msg.contains("zero"))
      }
    }

    "forall" - {
      "is true for all" - {
        verify(s).forall(_.nonEmpty).u
      }

      "is not true for one" - {
        val msg = interceptVerificationError { verify(s).forall(_.lengthCompare(3) == 0) }
        assert(msg.contains("three"))
        assert(msg.contains("did not satisfy"))
      }

      "is not true for many" - {
        val msg = interceptVerificationError { verify(s).forall(_.lengthCompare(5) == 0) }
        assert(msg.contains("one") || msg.contains("two"))
        assert(msg.contains("did not satisfy"))
      }
    }

    "setEquals" - {
      "dups in left" - {verify(List(1, 1, 2)).setEquals(List(1, 2)).u}
      "dups in right" - {verify(List(1, 2)).setEquals(List(1, 1, 2)).u}
      "dups in both" - {verify(List(1, 2, 2, 2)).setEquals(List(1, 1, 1, 2)).u}
      "extra in left" - {
        val msg = interceptVerificationError(verify(List(1, 2, 3)).setEquals(List(1, 2)))
        assert(msg == "1, 2, 3 contained extraneous 3")
      }
      "extra in right" - {
        val msg = interceptVerificationError(verify(List(1, 2)).setEquals(List(1, 2, 3)))
        assert(msg == "1, 2 does not contain 3")
      }


      // since scala's implementation of `Set` discriminates between sets of size <5 and >=5
      "using larger sets" - {
        "dups in left" - {verify(List(2, 3, 1, 1, 4, 5)).setEquals(List(4, 5, 3, 1, 2)).u}
        "dups in right" - {verify(List(2, 5, 1, 3, 4)).setEquals(List(5, 3, 2, 1, 1, 4)).u}
        "dups in both" - {verify(List(3, 2, 2, 2, 1, 5, 4)).setEquals(List(5, 3, 1, 2, 1, 4, 1)).u}

        "extra in left" - {
          val msg = interceptVerificationError(verify(List(1, 4, 5, 2, 3)).setEquals(List(2, 1, 4, 5)))
          assert(msg.contains("contained extraneous 3"))
        }
        "extra in right" - {
          val msg = interceptVerificationError(verify(List(2, 4, 5, 1)).setEquals(List(5, 2, 1, 3, 4)))
          assert(msg.contains("does not contain 3"))
        }

        "extra in both" - {
          val msg = interceptVerificationError { verify(List(1, 2, 3, 4, 5)).equals(List(2, 3, 4, 5, 6, 7)) }

          // notice that they're not equal
          assert( msg.contains("contained extra") || msg.contains("does not contain") )

          // report one of the differing elements
          assert( List(1, 6, 7).exists(x ⇒ msg.contains(x.toString)) )
        }
      }
    }


  }
}
