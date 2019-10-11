package ca.bwbecker.utest

import utest.ufansi
import utest.ufansi.{Attrs, Color, Str}

import scala.collection.mutable

/**
  * For automatically colouring a Verification Error.
  */
object AutoColor extends TruncateHelpers {
  val msgColor      : Attrs = Color.DarkGray
  val valueColor    : Attrs = Color.Blue
  val highlightColor: Attrs = Color.Red ++ ufansi.Bold.On

  val listSep: Str = msgColor(", ")

  /**
    * Internally used type for tagging values with what they should be colored as.
    * No public members.
    */
  abstract class ColoredComponent {
    private[AutoColor] def toStr: Str
  }

  /**
    * Quotes a `fansi.Str` to ensure that it is unchanged when printed.
    */
  case class Quoted(str: Str) extends ColoredComponent {
    override private[AutoColor]
    def toStr = str
  }

  /**
    * Colours a string using `msgColor`. Used to indicate "boilerplate" in an error message.
    */
  case class ColoredMsg(msg: String) extends ColoredComponent {
    override private[AutoColor]
    def toStr = msgColor(msg)
  }

  object ColoredList {
    def mkStr(strs: List[Str], sep: Str = listSep): Str = strs match {
      case Nil          ⇒ ""
      case head :: tail ⇒ tail.foldLeft(head) { (acc, s) ⇒ acc ++ sep ++ s }
    }
  }

  /**
    * Colours a list using `valueColor`. Inserts ", " in between elements.
    * Specific indices can be highlighted.
    *
    * @param ls A list.
    */
  case class ColoredList(ls: Iterable[_]) extends ColoredComponent {

    private var highlighted: Set[Int] = Set.empty

    override private[AutoColor]
    def toStr = {
      val colored: Iterable[Str] = for ((x, i) ← ls.zipWithIndex) yield
        if (highlighted(i))
          highlightColor(valueColor(x.toString))
        else
          valueColor(x.toString)

      truncEndStr(ColoredList.mkStr(colored.toList))
    }

    /**
      * Reassign the highlighted elements in this set. Mutates this ColoredComponent.
      * @param indices Which indices to highlight.
      */
    def highlight(indices: Set[Int]): ColoredList = {
      highlighted = indices
      this
    }

    def highlight(index: Int): ColoredList = {
      highlighted += index
      this
    }
  }

  /**
    * Colours the argument using `valueColor`. Used to indicate a literal value from the test.
    */
  case class ColoredValue(value: Any) extends ColoredComponent {
    override private[AutoColor]
    def toStr = valueColor(truncEnd(value.toString, len = DefaultContext.maxValCharacters))
  }

  /**
    * Colours the inner component using `highlightColor`. Used to highlight a value / message.
    */
  case class Highlighted(inner: ColoredComponent) extends ColoredComponent {
    // extend the inner toStr
    // with the current color scheme, this will effectively just replace
    override private[AutoColor]
    def toStr = highlightColor(inner.toStr)
  }

  import scala.language.implicitConversions

  // for messages
  implicit def String2ColoredComponent(s: String): ColoredComponent = ColoredMsg(s)

  // defaulted to not paint on top of a Str
  implicit def Str2ColoredComponent(s: Str): ColoredComponent = Quoted(s)

  // for lists
  implicit def Seq2ColoredComponent(l: Seq[_]): ColoredComponent = ColoredList(l)
  implicit def List2ColoredComponent(l: List[_]): ColoredComponent = ColoredList(l)
  implicit def Array2ColoredComponent(l: Array[_]): ColoredComponent = ColoredList(l)
  implicit def IndexedSe2ColoredComponent(l: IndexedSeq[_]): ColoredComponent = ColoredList(l)
  implicit def WrappedArray2ColoredComponent(l: mutable.WrappedArray[_]): ColoredComponent = ColoredList(l)

  // the rest are values

  // this may cause trouble
  // For example: a subtype of Seq would also be a subtype of AnyRef,
  // and the compiler will complain that there is an ambiguous conversion.
  implicit def AnyRef2ColoredComponent(r: AnyRef): ColoredComponent = ColoredValue(r)
  implicit def Any2ColoredComponent(a: Any): ColoredComponent = ColoredValue(a)


  /**
    * Provides short names for the constructors of ColoredComponents.
    * h = Highlighted is probably the most useful, since there are no implicit conversions for it.
    *
    * @note Provides: `m` , `v` , `h` , `l` , `q`.
    */
  object Abbreviations {
    val q: Str ⇒ Quoted                   = Quoted
    val m: String ⇒ ColoredMsg            = ColoredMsg
    val l: Iterable[_] ⇒ ColoredList      = (ls: Iterable[_]) ⇒ ColoredList(ls)
    val v: Any ⇒ ColoredValue             = ColoredValue
    val h: ColoredComponent ⇒ Highlighted = Highlighted
  }

  /**
    * Inserts arguments into a [[utest.ufansi.Str]], by colouring them according to their types. <br/>
    * Components are concatenated a space in between. (See `autoColorSep` to customize this.)
    * <br/><br/>
    *
    * See the implicit defs in [[ca.bwbecker.utest.AutoColor]] to find the default conversions,
    * and add to them as you see fit.
    * <br/><br/>
    *
    * If you'd like a `String` to be coloured as a value, please use `ColoredValue(myString)`.       <br/>
    * If you'd like a `fansi.Str` to be coloured as a message, please use `ColoredMessage(fansiStr)` <br/>
    * If you'd like a component to be highlighted, please use `Highlighted(myImportant)`             <br/>
    * Import [[ca.bwbecker.utest.AutoColor.Abbreviations]] to get shorter names for the constructors: `v`, `m`, `h`, `l`, `q`.
    * <br/><br/>
    *
    * Example: {{{autoColor("strings are messages",
    *
    *            'SymbolsAreValues,
    *            3.3.3, // Numbers are values too!
    *
    *            List('comma, 'separated, 'values), // Lists are inserted as values with commas
    *
    *            Highlighted("important message!"),
    *            h("shorthand for the above"),
    *
    *            ColoredValue("a string forced to be a value"),
    *            v("shorthand for the above"))}}}
    *
    * @param components The `ColoredComponents` to be added to the list.
    * @return A `fansi.Str` rendered as a `String`.
    */
  def autoColor(components: ColoredComponent*): String = autoColorSep(components: _*)(" ")

  /**
    * Inserts arguments into a [[utest.ufansi.Str]], by colouring them according to their types, <br/>
    * with a separator (coloured with `msgColor`) in between elements, defaulting to a space.
    * <br/><br/>
    *
    * See the implicit defs in [[ca.bwbecker.utest.AutoColor]] to find the default conversions,
    * and add to them as you see fit.
    * <br/><br/>
    *
    * If you'd like a `String` to be coloured as a value, please use `ColoredValue(myString)`.       <br/>
    * If you'd like a `fansi.Str` to be coloured as a message, please use `ColoredMessage(fansiStr)` <br/>
    * If you'd like a component to be highlighted, please use `Highlighted(myImportant)`             <br/>
    * Import [[ca.bwbecker.utest.AutoColor.Abbreviations]] to get shorter names for the constructors: `v`, `m`, `h`, `l`, `q`.
    *
    * Example: {{{autoColor("strings are messages",
    *
    *            'SymbolsAreValues,
    *            3.3.3, // Numbers are values too!
    *
    *            List('comma, 'separated, 'values), // Lists are inserted as values with commas
    *
    *            Highlighted("important message!"),
    *            h("shorthand for the above"),
    *
    *            ColoredValue("a string forced to be a value"),
    *            v("shorthand for the above"))}}}
    *
    * @param components The `ColoredComponents` to be added to the list.
    * @param sep        A separator, to be provided in a second parameter list.
    * @return A `fansi.Str` rendered as a `String`.
    */
  def autoColorSep(components: ColoredComponent*)(sep: String): String = components.toList match {
    case Nil          ⇒ ""
    case head :: tail ⇒
      val msgSep: Str = if (sep.forall(_.isWhitespace)) Str(sep) else msgColor(sep)
      tail
      .foldLeft(head.toStr) { (acc, comp) ⇒ acc ++ msgSep ++ comp.toStr }
      .render
  }
}
