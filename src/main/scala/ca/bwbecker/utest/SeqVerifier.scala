package ca.bwbecker.utest

import AutoColor.{ColoredComponent, autoColor}
import AutoColor.Abbreviations._

import scala.collection.GenTraversableOnce

class SeqVerifier[T](seq: Seq[T]) extends Verifier {

  /**
    * Highlights certain elements of `seq`, given a predicate.
    * @note Do not rely on this printing the sequence in the same order.
    *       There is a case to be made that it does reorder highlighted elements to the front.
    * @param is_highlighted Predicate that determines when to highlight.
    * @return A ColoredComponent with the elements in value color, and some highlighted.
    */
  private def highlightSeq(is_highlighted: T ⇒ Boolean): ColoredComponent = {
    val highlighted_indices = {
      for ((x, i) ← seq.zipWithIndex if is_highlighted(x))
      yield i
    }.toSet

    l(seq).highlight(highlighted_indices)
  }


  def isEmpty: SeqVerifier[T] = {
    if (seq.nonEmpty)
      throw VerificationError(seq, "is not empty.")
    this
  }

  def nonEmpty: SeqVerifier[T] = {
    if (seq.isEmpty)
      throw VerificationError(autoColor("Seq is empty."))
    this
  }

  /**
    * Verify that seq contains the provided item.
    */
  def contains(item: T): SeqVerifier[T] = {
    if (seq.contains(item)) {
      this
    } else {
      throw VerificationError(seq, "does not contain", item)
    }
  }

  def exists(pred: T ⇒ Boolean): SeqVerifier[T] = {
    if (seq.exists(pred)) {
      this
    } else {
      throw VerificationError(seq, "does not contain an element matching the predicate.")
    }
  }

  /**
    * Verify that seq does not contain any of the given items.
    */
  def containsNone(items: T*): SeqVerifier[T] = {
    val intersect = seq.intersect(items)
    if (intersect.nonEmpty) {
      throw VerificationError(
        highlightSeq(intersect.toSet.contains),
        "contains some of",
        intersect
      )
    }
    this
  }


  /**
    * Verify that seq contains  <b><em>exactly one</em></b>  of the given items.
    */
  def containsOne(items: T*): SeqVerifier[T] = {

    // first throw if we contain none
    containsSome(items: _*)

    val intersect = seq.intersect(items)

    if (intersect.length > 1) {
      throw VerificationError(
        highlightSeq(intersect.toSet.contains),
        "contains all of", intersect)
    }

    this
  }

  /**
    * Verify that seq contains one or more of the given items.
    */
  def containsSome(items: T*): SeqVerifier[T] = {
    val intersect = seq.intersect(items)
    if (intersect.isEmpty)
      throw VerificationError(seq, "contains none of", items)

    this
  }

  /**
    * Verify that seq contains ALL of the items.
    */
  def containsAll(items: T*): SeqVerifier[T] = {
    val diff = items.diff(seq)

    if (diff.nonEmpty)
      throw VerificationError(seq, "does not contain all of", diff)

    this
  }


  def forall(pred: T ⇒ Boolean): SeqVerifier[T] = {
    val noSatisfyMsg = "did not satisfy the predicate."

    seq.filterNot(pred) match {
      case Seq()           ⇒ this
      case Seq(x)          ⇒ throw VerificationError(x, noSatisfyMsg)
      case Seq(x, y)       ⇒ throw VerificationError(x, "and", y, noSatisfyMsg)
      case Seq(x, rest@_*) ⇒ throw VerificationError(x, "and", rest.length, "other items", noSatisfyMsg)
    }
  }

  // we might want to provide `update` to change this to a StringVerifier, and so on
  def update[U](f: Seq[T] ⇒ Seq[U]):            SeqVerifier[U] = new SeqVerifier[U](f(seq))
  def map[U](f: T ⇒ U):                         SeqVerifier[U] = new SeqVerifier[U](seq.map(f))
  def flatMap[U](f: T ⇒ GenTraversableOnce[U]): SeqVerifier[U] = new SeqVerifier[U](seq.flatMap(f))
  def filter(f: T ⇒ Boolean):                   SeqVerifier[T] = new SeqVerifier[T](seq.filter(f))


  private def unorderedEqualsImpl(seq: Seq[T], s2: Seq[T]): SeqVerifier[T] = {
    val onlyInMe  = seq.diff(s2)
    val onlyInYou = s2.diff(seq)

    if (onlyInYou.nonEmpty)
      throw VerificationError(this.seq, "does not contain", onlyInYou)

    if (onlyInMe.nonEmpty) {
      throw VerificationError(highlightSeq(onlyInMe.toSet.contains), "contained extraneous", onlyInMe)
    }

    this
  }

  /**
    * Verify that seq and s2 have the same elements, in any order. They must also contain the same number of duplicates.
    */
  def unorderedEquals(s2: Seq[T]) = unorderedEqualsImpl(this.seq, s2)

  private def cons2(x: T, y: T, s: Seq[T]): Seq[T] = x :: y :: s.toList

  /**
    * Verify that seq and the list of arguments have the same elements, in any order. They must also contain the same number of duplicates.
    * @note You <em>must</em> provide at least 2 arguments to this function. This is to avoid a double definition due to type erasure,
    *       but this makes sense anyways; use other functions if you only need to check 1 argument.
    */
  def unorderedEquals(s0: T, s1: T, s: T*)= unorderedEqualsImpl(this.seq, cons2(s0, s1, s))

  /**
    * Verify that seq and s2 have the same elements, in any order. Duplicates are ignored.
    */
  def setEquals(s2: Seq[T]) = unorderedEqualsImpl(this.seq.distinct, s2.distinct)

  /**
    * Verify that seq and the list of arguments have the same elements, in any order. Duplicates are ignored.
    * @note You <em>must</em> provide at least 2 arguments to this function. This is to avoid a double definition due to type erasure,
    *       but this makes sense anyways; use other functions if you only need to check 1 argument.
    */
  def setEquals(s0: T, s1: T, s: T*) = unorderedEqualsImpl(this.seq.toList.distinct, cons2(s0, s1, s).distinct)

  /**
    * Verify that seq and s2 have the same items in the same order.
    */
  def equals(s2: Seq[T]): SeqVerifier[T] = {
    if (seq.equals(s2)) {
      this
    } else {
      // first throw any errors from unorderedEquals
      unorderedEquals(s2)

      // otherwise, the error has to do with ordering
      // find the first element where the ordering differs
      val (commonPrefix, (seqElem, s2Elem) :: _) = seq.zip(s2).span { case (t1, t2) ⇒ t1 == t2 }

      throw VerificationError(seqElem,
        "differs from", s2Elem,
        "at position", commonPrefix.length
      )
    }
  }

  /**
    * Verify that seq and the list of arguments have the same items in the same order.
    * @note You <em>must</em> provide at least 2 arguments to this function. This is to avoid a double definition due to type erasure,
    *       but this makes sense anyways; use other functions if you only need to check 1 argument.
    */
  def equals(s0: T, s1: T, s: T*): SeqVerifier[T] = this.equals(cons2(s0, s1, s))

  /**
    * To ensure that the object itself isn't compared to other objects.
    * @param obj Anything
    * @return An Unimplemented Error.
    */
  override def equals(obj: Any): Boolean = ???

  /**
    * Verify that seq has the expected length.
    */
  def length(len: Int): SeqVerifier[T] = {
    if (seq.length == len) {
      this
    } else {
      throw VerificationError("length", seq.length, "!=", len)
    }
  }

  /**
    * Verify that seq has the expected length.
    */
  def lengthAtLeast(minLen: Int): SeqVerifier[T] = {
    if (seq.length >= minLen) {
      this
    } else {
      throw VerificationError("length of seq =", seq.length, "not greater than", minLen)
    }
  }

  /**
    * Verify that seq has the expected length.
    */
  def lengthAtMost(maxLen: Int): SeqVerifier[T] = {
    if (seq.length <= maxLen) {
      this
    } else {
      throw VerificationError("length of seq =", seq.length, "not less than", maxLen)
    }
  }

  /**
    * Verify that seq has the expected length.
    */
  def lengthBetween(minLen: Int, maxLen: Int): SeqVerifier[T] = {
    this.lengthAtLeast(minLen).lengthAtMost(maxLen)
  }

}
