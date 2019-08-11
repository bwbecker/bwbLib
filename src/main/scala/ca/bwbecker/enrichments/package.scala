package ca.bwbecker

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
      * Transform a boolean to an option.  Usage:
      * myBool.option(result) => Some(result) if myBool is true; None otherwise
      *
      * @param a
      * @tparam A
      * @return
      */
    final def option[A](a: => A): Option[A] = if (b) Some(a) else None
  }



  implicit class RichTraversable[A, C[A] <: Iterable[A]](ca: C[A]) {

    import collection.generic.CanBuildFrom

    /**
      * Return a copy of the list but with the first element matching item removed.
      */
    def less(item: A)(implicit cbf: CanBuildFrom[C[A], A, C[A]]): C[A] = {
      val it = ca.iterator
      val result = cbf()
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


}
