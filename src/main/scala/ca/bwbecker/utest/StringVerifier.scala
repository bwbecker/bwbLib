package ca.bwbecker.utest

import scala.util.matching.Regex
import ca.bwbecker.enrichments.RichString

class StringVerifier(s: String) extends Verifier {

  import AutoColor.Abbreviations.{q, v}

  /**
    * Verify that s2 is exactly equal to s.
    */
  def equals(s2: String): StringVerifier = {
    if (s.equals(s2)) {
      this
    } else {
      throw VerificationError(stringDiff(s, s2))
    }
  }

  /**
    * Verify that s contains the substring s2
    */
  def contains(t: String): StringVerifier = {
    val context = 20

//    def withContext(str: String, lcs: String): String = {
//      val p = str.indexOf(lcs)
//      str.substring(Math.max(0, p - context),
//        Math.min(p + lcs.length + context, str.length))
//    }

    if (s.contains(t)) {
      this
    } else {
      val lcs = this.longestCommonSubSeq(this.s, t).mkString("")

      if (t.startsWith(lcs)) {
        val (tLcs, tSuf) = t.splitAt(lcs.length)
        val (sLcs, sSuf) = this.s.splitAt(this.s.indexOf(lcs) + lcs.length)
        throw VerificationError(
          q(truncStart(sLcs) + truncEnd(sSuf, tSuf.length)),
          "did not contain",
          v(truncStart(tLcs) + truncEnd(tSuf))
        )

      } else if (t.endsWith(lcs)) {
        val (tPre, tLcs) = t.splitAt(t.indexOf(lcs))
        val (sPre, sLcs) = this.s.splitAt(this.s.indexOf(lcs))
        throw VerificationError(
          q(truncStart(sPre) + truncEnd(sLcs)),
          "did not contain",
          v(truncStart(tPre) + truncEnd(tLcs))
        )
      } else {
        val (tPre, tLcs, tSuf) = this.partition(t, t.indexOf(lcs), lcs.length)
        val (sPre, sLcs, sSuf) = this.partition(this.s, this.s.indexOf(lcs), lcs.length)
        throw VerificationError(
          q(truncStart(sPre) + truncMiddle(sLcs) + truncEnd(sSuf)),
          "did not contain",
          q(truncStart(tPre) + truncMiddle(tLcs) + truncEnd(tSuf))
        )
      }

    }
  }

  /**
    * Verify that `s` contains the substring `s2`, ignoring case.
    */
  def containsInsensitive(s2: String): StringVerifier = {
    val s = this.s.toLowerCase
    val str2 = s2.toLowerCase

    if (s.contains(str2)) {
      this
    } else {
      val lcs = this.longestCommonSubSeq(s, str2)
      val pos = s.indexOf(lcs)
      val (pre, mid, suf) = this.partition(s, pos, lcs.length)

      throw VerificationError(
        q(formatTriple(pre, mid, suf)),
        "did not contain",
        v(str2)
      )
    }
  }

  def containsAll(strs: String*): StringVerifier = {
    val notContained = strs.filterNot(s.contains)
    if (notContained.isEmpty) {
      this
    } else {
      throw VerificationError(v(this.s.cap(20)), "did not contain all of", notContained.mkString("{", ", ", "}"))
    }
  }

  def containsNone(strs: String*): StringVerifier = {
    val contained = strs.filter(s.contains)
    if (contained.isEmpty) {
      this
    } else {
      throw VerificationError(v(this.s.cap(20)), "contains some of", contained.mkString("{", ", ", "}"))
    }
  }

  /**
    * Verify that s2 starts with s.
    */
  def startsWith(s2: String): StringVerifier = {
    if (s.startsWith(s2)) {
      this
    } else {

      // check if the string is even contained
      // this will throw its own error talking about containment if it fails
      this.contains(s2).u

      // if the containment check passes, then

      val pos = this.s.indexOf(s2)
      val (pre, mid, suf) = this.partition(s, pos, s2.length)

      throw VerificationError(
        q(formatTriple(pre, mid, suf)),
        "contained", v(s2),
        "at index", pos
      )
    }
  }

  def lengthAtLeast(minLen: Int): StringVerifier = {
    if (s.length >= minLen)
      this
    else
      throw VerificationError(
        "length of string =", s.length,
        "not greater than", minLen
      )
  }

  def lengthAtMost(maxLen: Int): StringVerifier = {
    if (s.length <= maxLen)
      this
    else
      throw VerificationError(
        "length of string =", s.length,
        "not less than", maxLen
      )
  }

  def matchesRE(re: Regex): StringVerifier = {
    if (re.findFirstMatchIn(s).isEmpty)
      throw VerificationError(
        v(s),
        "did not match",
        re.pattern
      )

    this
  }

}
