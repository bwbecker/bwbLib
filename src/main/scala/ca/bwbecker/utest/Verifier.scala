package ca.bwbecker.utest

import utest.ufansi.Str

private[utest] trait TruncateHelpers {
  val dots = "..."

  protected object DefaultContext {
    val maxValCharacters = 40 // how many characters of `Values` or important strings we should print
    val maxMsgCharacters = 25 // how many characters of boiler-plate messages we should print
    val maxItems         = 10 // how many elements of a list to print (currently unused)
  }

  import DefaultContext._

  def truncStart(s: String, len: Int = maxMsgCharacters, signifier: String = dots): String = {
    if (s.length > len) {
      signifier + s.takeRight(len - signifier.length)
    } else {
      s
    }
  }

  def truncEnd(s: String, len: Int = maxMsgCharacters, signifier: String = dots): String = {
    if (s.length > len) {
      s.take(len - signifier.length) + signifier
    } else {
      s
    }
  }

  def truncMiddle(s:String, len:Int = maxMsgCharacters, signifier:String = dots):String = {
    if (s.length > len) {
      val leftLen = (len - signifier.length)/2
      val rightLen = len - signifier.length - leftLen
      s.take(leftLen) + signifier + s.takeRight(rightLen)
    } else {
      s
    }
  }

  // for Str (since we need to use Str.substring
  def truncEndStr(s: Str, len: Int = maxMsgCharacters, signifier: String = dots): Str = {
    if (s.plainText.contains("LONG LONG LONG LONG")) {
      throw new Exception("hey stacktrace")
    }

    if (s.length > len) {
      s.substring(0, len - signifier.length) ++ AutoColor.msgColor(signifier)
    } else {
      s
    }
  }
}

trait Verifier extends TruncateHelpers {

  protected val lMarker        = "["
  protected val rMarker        = "]"


  /**
    * Partition a string into three parts:  a middle value, the prefix, and
    * the suffix.  The value is from position start to position start+len.
    */
  def partition(s: String, start: Int, len: Int): (String, String, String) = {
    val (pre, rest) = s.splitAt(start)
    val (v, suff) = rest.splitAt(len)
    (pre, v, suff)
  }

  import AutoColor.{autoColor, autoColorSep}
  import AutoColor.Abbreviations.{v, h, q}

  /**
    * Format a triple (p, v, s) with valueColor, and with v highlighted.
    */
  def formatTriple(prefix: String, value: String, suffix: String): String = {
    autoColorSep(
      v(truncStart(prefix)), // treat prefix and suffix as "boilerplate" messages
      h(v(truncMiddle(value))),
      v(truncEnd(suffix))
    )("")
  }

  private def get_commonPrefix[T](left: Seq[T], right: Seq[T]): Seq[T] = {
    val (left_prf, right_prf) = left.zip(right).takeWhile { case (a, b) ⇒ a == b }.unzip
    left_prf // they should be equal anyways
  }

  /**
    * Compare two strings to find and highlight in [...] where they differ.
    */
  def stringDiff(s1: String, s2: String): Str = {

    val left = get_commonPrefix(s1, s2).mkString

    // this ensures the common prefix and common suffix doesn't overlap
    def reverse_rest(s: String) = s.drop(left.length).reverse

    val right = get_commonPrefix(reverse_rest(s1), reverse_rest(s2)).reverse.mkString

    def get_middle(s: String): String = s.substring(left.length, s.length - right.length)

    autoColor(
      q(formatTriple(left, lMarker + get_middle(s1) + rMarker, right)),
      "!=",
      q(formatTriple(left, lMarker + get_middle(s2) + rMarker, right))
    )
  }

  /**
    * Longest common subsequence.  From
    * https://stackoverflow.com/questions/44672145/functional-way-to-find-the-longest-common-substring-between-two
    * -strings-in-scala
    */
  protected def longestCommonSubSeq[T](a: IndexedSeq[T], b: IndexedSeq[T]): IndexedSeq[T] = {
    @scala.annotation.tailrec
    def loop(bestLengths: Map[(Int, Int), Int], bestIndices: (Int, Int), i: Int, j: Int): IndexedSeq[T] = {
      if (i > a.length) {
        val bestJ = bestIndices._2
        b.slice(bestJ - bestLengths(bestIndices), bestJ)
      } else {
        val currentLength = if (a(i - 1) == b(j - 1)) bestLengths((i - 1, j - 1)) + 1 else 0
        loop(
          bestLengths + ((i, j) -> currentLength),
          if (currentLength > bestLengths(bestIndices)) (i, j) else bestIndices,
          if (j == b.length) i + 1 else i,
          if (j == b.length) 1 else j + 1)
      }
    }

    loop(Map.empty[(Int, Int), Int].withDefaultValue(0), (0, 0), 1, 1)
  }

  /**
    * uTest prints the last value unless it's a unit.  Make it easy to return a unit.
    */
  def u: Unit = Unit

}
