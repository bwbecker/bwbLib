package ca.bwbecker.utest

class VerifierPrecision(val d:Double)


class DoubleVerifier(d:Double) extends Verifier {

  import AutoColor.autoColor

  def equals(j: Double)(implicit precision:VerifierPrecision = new VerifierPrecision(0.0003)): DoubleVerifier = {
    if (Math.abs(this.d - j) > precision.d)
      throw VerificationError(autoColor(d, "!=", j, s"+/- ${precision.d}"))

    this
  }

  def greaterThan(j: Double): DoubleVerifier = {
    if (this.d > j) {
      this
    } else {
      throw VerificationError(autoColor(d, "not >", j))
    }
  }

  def lessThan(j: Double): DoubleVerifier = {
    if (this.d < j) {
      this
    } else {
      throw VerificationError(autoColor(d, "not <", j))
    }
  }

}


