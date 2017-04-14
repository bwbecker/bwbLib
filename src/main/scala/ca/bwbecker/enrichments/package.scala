package ca.bwbecker

/**
  * Created by bwbecker on 2016-06-17.
  */
package object enrichments {


  import scala.util.matching.Regex
  import scala.language.implicitConversions


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
      * Transform a boolean to an option.  Usage:
      * myBool.option(result) => Some(result) if myBool is true; None otherwise
      *
      * @param a
      * @tparam A
      * @return
      */
    final def option[A](a: => A): Option[A] = if (b) Some(a) else None
  }


  implicit class RichTraversable[T](val seq: TraversableOnce[T]) {

    /**
      * Sum values in a traversage that are extracted with f.
      */
    def sumBy[Res](f: T => Res)(implicit num: Numeric[Res]) = {
      seq.foldLeft(num.zero)((acc, b) ⇒ num.plus(acc, f(b)))
    }
  }


}
