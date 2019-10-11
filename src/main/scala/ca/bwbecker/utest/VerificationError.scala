package ca.bwbecker.utest

import ca.bwbecker.utest.AutoColor.{ColoredComponent, autoColor}

class VerificationError(msg: String) extends utest.AssertionError(msg, Seq())
  with OatTestFramework.FansiErrorMarker {
}


object VerificationError {

  def apply(msg: String) = new VerificationError(msg)

  def apply(msg: utest.ufansi.Str) = new VerificationError(msg.render)

  def apply(c1: ColoredComponent, c2: ColoredComponent, comps: ColoredComponent*): VerificationError = {
    val all_comps = c1 :: c2 :: comps.toList
    new VerificationError(autoColor(all_comps: _*))
  }
}