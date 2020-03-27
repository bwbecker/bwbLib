package ca.bwbecker

import scala.concurrent.duration.FiniteDuration

import java.time.{LocalDate, LocalTime}

/**
  * Created by bwbecker on 2016-06-17.
  */
package object enrichments {


  import scala.util.matching.Regex
  import scala.language.higherKinds

  /**
    * Pimp the Regex class to include a method to just check if a string
    * matches the regular expression.
    *
    * Usage: {{{
    * import Enrichments.RichRegex
    * val Digit = """\d""".r
    * if (Digit matches "5") println("match")
    * else println("no match")
    * }}}
    *
    * @author mkneissl via http://stackoverflow
    *         .com/questions/3021813/how-to-check-whether-a-string-fully-matches-a-regex-in-scala
    *
    */
  implicit class RichRegex(underlying: Regex) extends AnyRef {

    /**
      * Does this Regex match s?
      */
    def matches(s: String): Boolean = this.underlying.pattern.matcher(s).matches

    /**
      * Does this Regex miss (not match) s?
      */
    def misses(s: String): Boolean = !this.matches(s)
  }


  /**
    * Enrich the Boolean class
    *
    * @param b
    */
  implicit class RichBoolean(val b: Boolean) extends AnyVal {

    /**
      * Transform a boolean to an option.
      *
      * Examples:
      * true.option("A string") => Some("A string)
      * false.option("A string") => None
      *
      * @param a
      * @tparam A
      * @return
      */
    final def option[A](a: => A): Option[A] = if (b) Some(a) else None

    /**
      * An implementation of the ternary operator.
      *
      * Usage:
      * {{{
      *   import ca.bwbecker.enrichments.RichBoolean
      *   val condition:Boolean = true
      *
      *   val x = condition ? "yes" | "no"
      * }}}
      */
    def ?[X](t: ⇒ X): Ternary[X] = new Ternary[X] {
      def |(f: ⇒ X) = if (b) t else f
    }
  }

  trait Ternary[X] {
    def |(f: ⇒ X): X
  }


  /**
    * Enrich the String class.
    *
    * @param s
    */
  implicit class RichString(val s: String) extends AnyVal {

    /**
      * Transform a string into an Option[String].
      */
    final def toOption: Option[String] = if (s == null || s == "") None else Some(s)

    /**
      * Fallback for an empty string.
      * Examples:
      * "a string".getOrElse("fallback") ==> "a string"
      * "".getOrElse("fallback") ==> "fallback"
      */
    final def getOrElse(value: String): String = {
      if (s.isEmpty) value else s
    }

    /**
      * Can a string to a given length, adding an ellipses if capped.
      * Examples:
      * "abc".cap(3) => "abc"
      * "abcd".cap(3) => "ab…"
      */
    final def cap(max: Int): String = {
      if (s.length > max) s.take(max - 1) + "…" else s
    }

  }


  /**
    * Extend IndexedSeq (principally Vector) with additional methods.
    *
    * Usage: {{{
    * import Enrichments.RichIndexedSeq
    * val v1 = Vector(0, 1, 2, 3)
    * println(v1.withoutIndex(2)) ==> Vector(0, 2, 3)
    * }}}
    */
  implicit class RichVector[T](val v: Vector[T]) extends AnyVal {
    /*
    Note:  It would be great to make this apply to more collection types.  There are helpful posts
    on stackoverflow (http://stackoverflow.com/questions/5410846/how-do-i-apply-the-enrich-my-library-pattern-to
    -scala-collections)
    and in the Scala documentation itself (eg: http://www.scala-lang
    .org/api/current/scala/collection/generic/IsTraversableLike.html),
    however I was not able to get it to work in the time available.
     */

    /**
      * Return a copy of this vector with the first instance of elem removed.
      */
    def without(elem: T): Vector[T] = {
      val i = v.indexOf(elem)
      withoutIndex(i)
    }

    /**
      * Return a copy of this vector, omitting the element at index i.
      */
    def withoutIndex(i: Int): Vector[T] = {
      if (0 <= i && i < this.v.length) {
        val (front, back) = this.v.splitAt(i)
        front ++ back.drop(1)
      } else {
        this.v
      }
    }

    /**
      * Shuffle the order of this vector.
      *
      * @return
      */
    def shuffle: Vector[T] = util.Random.shuffle(v)

  }


  implicit class RichTraversable[A, C[A] <: Iterable[A]](ca: C[A]) {

    import collection.generic.CanBuildFrom

    /**
      * Return a copy of the list but with the first element matching item removed.
      */
    def less(item: A)(implicit cbf: CanBuildFrom[C[A], A, C[A]]): C[A] = {
      val it        = ca.iterator
      val result    = cbf()
      var foundItem = false

      while (it.hasNext) {
        val e = it.next()
        if (e == item && !foundItem) {
          foundItem = true
        } else {
          result += e
        }
      }
      result.result()
    }


    /**
      * Sum values in a traversage that are extracted with f.
      */
    def sumBy[Res](f: A => Res)(implicit num: Numeric[Res]) = {
      ca.foldLeft(num.zero)((acc, b) ⇒ num.plus(acc, f(b)))
    }


    /**
      * Return the {min, max} of the list along with all the other elements.
      * List must have at least one element.
      */
    def singleOut(f: (A, A) ⇒ Boolean)(implicit cbf: CanBuildFrom[C[A], A, C[A]]): (A, C[A]) = {
      assert(ca.nonEmpty, "singleOut applied to empty traversable.")
      val it        = ca.iterator
      var selected  = it.next()
      val result    = cbf()
      var foundItem = false

      while (it.hasNext) {
        val e = it.next()
        if (f(e, selected)) {
          result += selected
          selected = e
        } else {
          result += e
        }
      }
      (selected, result.result())
    }

  }


  /**
    * Stuff to test double values to within a given precision
    *
    * Usage:
    * import ca.bwbecker.enrichments.RichDouble
    * implicit val precision = Precision(0.0001)
    * assert(1.0000 ~= 1.00005) ==> true
    */
  implicit class RichDouble(d: Double) {

    /**
      * Do two double precision numbers differ by no more than
      * the given precision?
      */
    def ~=(d2: Double)(implicit p: Precision): Boolean = Math.abs(d - d2) <= p.p
  }

  case class Precision(val p: Double)


  /**
    * Comparison enrichments for java.time.LocalTime.
    */
  implicit class RichLocalTime(val t: LocalTime) extends AnyVal {
    def +(d: FiniteDuration): LocalTime = {
      t.plusNanos(d.toNanos)
    }

    def >(rhs: LocalTime): Boolean = t.isAfter(rhs)

    def <(rhs: LocalTime): Boolean = t.isBefore(rhs)

    def ≤(rhs: LocalTime): Boolean = !t.isAfter(rhs)
  }

  /**
    * Comparison enrichments for java.time.LocalDate
    */
  implicit class RichLocalDate(val t: LocalDate) extends AnyVal {

    def >(rhs: LocalDate): Boolean = t.isAfter(rhs)

    def <(rhs: LocalDate): Boolean = t.isBefore(rhs)

    def ≤(rhs: LocalDate): Boolean = !t.isAfter(rhs)
  }


}
