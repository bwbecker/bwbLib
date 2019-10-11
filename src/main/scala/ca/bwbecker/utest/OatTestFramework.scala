package ca.bwbecker.utest

import utest._
import utest.framework.StackMarker

import scala.collection.mutable


class OatTestFramework extends utest.runner.Framework {
  private val omitStackFrameRE = "^((scala|akka|anorm|play)\\..*|utest\\.asserts.*|oat\\.xplatform\\.utest.*)".r

  def exceptionIncludeStackTraceElement(s: StackTraceElement): Boolean = {
    omitStackFrameRE.findFirstIn(s.getClassName).isEmpty
  }

  override def showSummaryThreshold = 3

  // This prints the tree view.  Doesn't seem intuitive.
  override def useSbtLoggers = true


  //  override def testValueColor = toggledColor(ufansi.Color.Blue)
  //  override def exceptionClassColor = toggledColor(ufansi.Underlined.On ++ ufansi.Color.LightRed)
  //  override def exceptionMsgColor = toggledColor(ufansi.Color.LightRed)
  //  override def exceptionPrefixColor = toggledColor(ufansi.Color.Red)
  //  override def exceptionMethodColor = toggledColor(ufansi.Color.LightRed)
  //  override def exceptionPunctuationColor = toggledColor(ufansi.Color.Red)
  //  override def exceptionLineNumberColor = toggledColor(ufansi.Color.LightRed)
  //  override def exceptionStackFrameHighlighter(s: StackTraceElement) = true


  override def exceptionMsgColor = toggledColor(ufansi.Color.Red)

  override def exceptionMethodColor = toggledColor(ufansi.Color.Blue)

  override def exceptionPrefixColor = toggledColor(ufansi.Color.LightBlue)

  override def exceptionClassColor = toggledColor(ufansi.Underlined.On ++ ufansi.Color.LightRed)


  // bwb:  Copied for utest.runner.Framework.
  override def formatException(x: Throwable, leftIndent: String) = {
    // bwb
    //x.setStackTrace(x.getStackTrace.filterNot(ste ⇒ stackTraceFilter.findFirstIn(ste.getClassName).isDefined))

    val output = mutable.Buffer.empty[ufansi.Str]
    var current = x
    while (current != null) {
      val exCls = exceptionClassColor(current.getClass.getName)
      output.append(
        joinLineStr(
          lineWrapInput(
            current.getMessage match {
              case null => exCls
              //case nonNull => ufansi.Str.join(exCls, ": ", exceptionMsgColor(nonNull))
              case nonNull =>
                current match {
                  case e: OatTestFramework.FansiErrorMarker ⇒ ufansi.Str.join(exCls, ": ", nonNull)
                  case e                                    ⇒ ufansi.Str.join(exCls, ": ", exceptionMsgColor(nonNull))
                }
            },
            leftIndent
          ),
          leftIndent
        )
      )

      // bwb:  Filter undesired stack frames
      //val stack = current.getStackTrace
      val stack = current.getStackTrace.filter(exceptionIncludeStackTraceElement)


      StackMarker.filterCallStack(stack)
        .foreach { e =>
          // Scala.js for some reason likes putting in full-paths into the
          // filename slot, rather than just the last segment of the file-path
          // like Scala-JVM does. This results in that portion of the
          // stacktrace being terribly long, wrapping around and generally
          // being impossible to read. We thus manually drop the earlier
          // portion of the file path and keep only the last segment

          val filenameFrag: ufansi.Str = e.getFileName match {
            case null     => exceptionLineNumberColor("Unknown")
            case fileName =>
              val shortenedFilename = fileName.lastIndexOf('/') match {
                case -1 => fileName
                case n  => fileName.drop(n + 1)
              }
              ufansi.Str.join(
                exceptionLineNumberColor(shortenedFilename),
                ":",
                exceptionLineNumberColor(e.getLineNumber.toString)
              )
          }

          val frameIndent = leftIndent + "  "
          val wrapper =
            if (exceptionStackFrameHighlighter(e)) ufansi.Attrs.Empty
            else ufansi.Bold.Faint

          output.append(
            "\n", frameIndent,
            joinLineStr(
              lineWrapInput(
                wrapper(
                  ufansi.Str.join(
                    exceptionPrefixColor(e.getClassName + "."),
                    exceptionMethodColor(e.getMethodName),
                    exceptionPunctuationColor("("),
                    filenameFrag,
                    exceptionPunctuationColor(")")
                  )
                ),
                frameIndent
              ),
              frameIndent
            )
          )
        }
      current = current.getCause
      if (current != null) output.append("\n", leftIndent)
    }

    ufansi.Str.join(output.toSeq: _*)
  }

}


object OatTestFramework {

  trait FansiErrorMarker {}

}