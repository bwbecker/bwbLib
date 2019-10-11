package ca.bwbecker.utest.tests

import utest._
import utest.ufansi.Str
import ca.bwbecker.utest._

import utest.framework.TestPath


object VerifierHelperTests extends TestSuite with Verifier {

  import AutoColor.{valueColor, highlightColor, msgColor}

  val tests = Tests {
    "longest common substring" - {

      "substring" - {
        assert(longestCommonSubSeq("173_abbabcbabbc_932", "414_babcbabcbababc_2131") == "babcbab".toSeq)
      }

      "where s2 is properly contained in s1" - {
        "equal" - {longestCommonSubSeq("s1", "s1") ==> "s1".toSeq}
        "at begin" - {longestCommonSubSeq("xxxMoxxre", "xxx") ==> "xxx".toSeq}
        "at end" - {longestCommonSubSeq("Moxxrexxx", "xxx") ==> "xxx".toSeq}
        "middle" - {longestCommonSubSeq("MoxxrexxxRexxst", "xxx") ==> "xxx".toSeq}
      }
    }

    "partition a string" - {
      "basic" - {
        this.partition("abcXXXdef", 3, 3) ==> ("abc", "XXX", "def")
      }
      "basic longer" - {
        this.partition("abcXXXdefghi", 3, 3) ==> ("abc", "XXX", "defghi")
      }
      "empty prefix" - {
        this.partition("XXXdef", 0, 3) ==> ("", "XXX", "def")
      }
      "empty suffix" - {
        this.partition("abcXXX", 3, 3) ==> ("abc", "XXX", "")
      }
    }

    "truncStart" - {
      "short" - {truncStart("12345", 15) ==> "12345"}
      "bang on" - {truncStart("12345", 5) ==> "12345"}
      "long" - {truncStart("1234567890", 5) ==> "...90"}
    }

    "truncEnd" - {
      "short" - {truncEnd("12345", 15) ==> "12345"}
      "bang on" - {truncEnd("12345", 5) ==> "12345"}
      "long" - {truncEnd("1234567890", 5) ==> "12..."}
      "longer" - {truncEnd("123456789012", 5) ==> "12..."}
    }

    "stringDiff" - {
      "middle of short string" - {
        stringDiff("abcXXXdef", "abcYYYdef").plainText ==> "abc[XXX]def != abc[YYY]def"
      }
      "middle of long string" - {
        val s1  = "012345678901234567890123456789xxx012345678901234567890123456789"
        val s2  = "012345678901234567890123456789yyy012345678901234567890123456789"
        val msg = stringDiff(s1, s2).plainText
        assert(msg.contains("[xxx]"))
        assert(msg.contains("[yyy]"))
        assert(msg.contains(dots))
      }
      "middle of long string; differing lengths" - {
        val s1  = "012345678901234567890123456789xxxxxx012345678901234567890123456789"
        val s2  = "012345678901234567890123456789yyy012345678901234567890123456789"
        val msg = stringDiff(s1, s2).plainText
        assert(msg.contains("[xxxxxx]"))
        assert(msg.contains("[yyy]"))
        assert(msg.contains(dots))
      }

      "middle of long string (fansi)" - {
        val s1       = "012345678901234567890123456789xxx012345678901234567890123456789"
        val s2       = "012345678901234567890123456789yyy012345678901234567890123456789"
        val msg      = stringDiff(s1, s2)
        val plain    = msg.plainText
        val xStart   = plain.indexOf("[xxx]")
        val dotStart = plain.lastIndexOf("...")

        msg.substring(xStart, xStart + 5) ==> highlightColor("[xxx]")
        msg.substring(0, 3) ==> valueColor("...")
        msg.substring(dotStart, dotStart + 3) ==> valueColor("...")
        assert(msg.length < s1.length + s2.length)
      }

      "automated colouring" - {
        import ufansi.Underlined

        import AutoColor.ColoredList.mkStr
        import AutoColor.{ColoredComponent, autoColor, ColoredList, ColoredMsg, ColoredValue, Highlighted, Quoted}
        import AutoColor.Abbreviations._

        val ls1 = List("a", "b", "c")
        val ls2 = List('also, 'important, 'symbols)

        val expected_ls1 = mkStr(ls1.map(s ⇒ valueColor(s)))
        val expected_ls2 = mkStr(ls2.map(s ⇒ valueColor(s.toString)))

        "autoColor should" - {
          val input1: List[ColoredComponent] = List(
            ColoredMsg("a message"),
            ColoredValue(17),
            ColoredValue(3.8),
            ColoredList(ls1),
            Highlighted(ColoredValue('ImportantSymbol)),
            Highlighted(ColoredMsg("Important too!")),
            Highlighted(ColoredList(ls2)),
            Quoted(Underlined.On("just underlined")),
          )

          val expected: String = {
            msgColor("a message") ++ " " ++
              valueColor("17") ++ " " ++
              valueColor("3.8") ++ " " ++
              expected_ls1 ++ " " ++
              highlightColor(valueColor(truncEnd("'ImportantSymbol"))) ++ " " ++
              highlightColor(msgColor("Important too!")) ++ " " ++
              highlightColor(truncEndStr(expected_ls2)) ++ " " ++
              Underlined.On("just underlined")
            }.render

          "correctly colour all ColoredComponents" - assert(autoColor(input1: _*) == expected)

          "allow for use of abbreviations" - {
            val input: List[ColoredComponent] = List(
              m("a message"),
              v(17),
              v(3.8),
              l(List("a", "b", "c")),
              h(v('ImportantSymbol)),
              h(m("Important too!")),
              h(l(List('also, 'important, 'symbols))),
              q(Underlined.On("just underlined")),
            )

            assert(autoColor(input: _*) == expected)
          }

          "implicit conversions should" - {

            // forces any implicit conversions to ColoredComponent
            def conversionTest(x: ColoredComponent, pf: PartialFunction[ColoredComponent, Unit])(implicit
                                                                                                 testPath: TestPath)
            : Unit = {
              val expected_string = testPath.value.last.split(" ").last
              pf.applyOrElse(
                x,
                { x: ColoredComponent ⇒ Predef.assert(false, s"Expected $x to be a $expected_string.") }
              )
            }

            "convert an Int to a ColoredValue" - conversionTest(35, { case x: ColoredValue ⇒
              assert(autoColor(x) == autoColor(v("35")))
            })

            "convert a String to a ColoredMsg" - conversionTest("helloworld", { case msg: ColoredMsg ⇒
              assert(autoColor(msg) == autoColor(m("helloworld")))
            })

            val expected_lsNum = mkStr(List("1", "2", "3").map(valueColor(_)))

            "convert a List to a ColoredList" - conversionTest(List(1, 2, 3), { case ls: ColoredList ⇒
              assert(autoColor(ls) == autoColor(expected_lsNum))
            })

            "convert VarArgs to a ColoredList" - {
              def test[T](args: T*): ColoredComponent = args

              conversionTest(test(1, 2, 3), { case ls: ColoredList ⇒
                assert(autoColor(ls) == autoColor(expected_lsNum))
              })
            }

            "convert a fansi.Str to a Quoted" - {
              conversionTest(Underlined.On(Str("helloworld")), { case qstr@Quoted(underlinedStr) ⇒
                // preferably, I'd like to call a function like "isUnderlined" instead
                assert(autoColor(qstr) == underlinedStr.render)
              })
            }

            "convert objects to a ColoredValue" - {
              object TestObject {override def toString = "TestObject"}
              case object TestCaseObject {override def toString = "TestCaseObject"}
              class TestClass {override def toString = "TestClass"}
              case class TestCaseClass() {override def toString = "TestCaseClass"}

              for {obj ← List(
                TestObject,
                TestCaseObject,
                new TestClass,
                TestCaseClass()
              )
                   }
                conversionTest(obj, { case x: ColoredValue ⇒
                  assert(autoColor(x) == autoColor(v(obj.toString)))
                })
            }

            "convert all arguments to autoColor" -
              assert(
                autoColor("a message",
                  17,
                  3.8,
                  List("a", "b", "c"),
                  h('ImportantSymbol),
                  h("Important too!"),
                  h(List('also, 'important, 'symbols)),
                  Underlined.On("just underlined")
                ) == expected)
          }
        }
      }
    }
  }
}
