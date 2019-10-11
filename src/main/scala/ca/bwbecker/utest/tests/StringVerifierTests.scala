package ca.bwbecker.utest.tests

import ca.bwbecker.utest.{VerificationError, Verify}

import utest._
import utest.ufansi.Str


object StringVerifierTests extends TestSuite with Verify {

  val tests = Tests {
    val s = "Here is a string to check"
    val v = verify(s)

    "equals" - {
      "equal" - {
        v.equals(s) ==> v
      }

      "not equal" - {
        val e = intercept[VerificationError] {v.equals("Here IS a string to check")}
        val msg = Str(e.getMessage).plainText
        assert(msg.contains("[is]"))
        assert(msg.contains("[IS]"))
      }
    }

    "contains" - {
      "does contain" - {
        v.contains("string") ==> v
      }

      "does not contain" - {
        val e = intercept[VerificationError] {v.contains("String")}
        val msg = Str(e.getMessage).plainText
        assert(msg.contains("String"))
        assert(msg.contains("string"))
        assert(msg.contains("did not contain"))
      }

      "insensitive" - {
        v.containsInsensitive("STRING") ==> v
        v.containsInsensitive("string") ==> v

        val e = intercept[VerificationError] {v.contains("St-ring")}
        val msg = Str(e.getMessage).plainText
        assert(msg.contains("St-ring"))
        assert(msg.contains("string"))
        assert(msg.contains("did not contain"))
      }

      "contains all" - {
        v.containsAll("Here", "a", "check") ==> v

        val e = intercept[VerificationError] {v.containsAll("Here", "NOT", "a", "test")}
        val msg = Str(e.getMessage).plainText
        msg ==> "Here is a string to… did not contain all of {NOT, test}"
      }

      "contains none" - {
        v.containsNone("HERE", "THERE") ==> v

        val e = intercept[VerificationError]{v.containsNone("HERE", "string", "check")}
        val msg = Str(e.getMessage).plainText
        msg ==> "Here is a string to… contains some of {string, check}"
      }
    }
  }
}
