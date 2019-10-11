package ca.bwbecker.utest

class IntVerifier(i: Int) extends Verifier {

  import AutoColor.autoColor

  def equals(j: Int): IntVerifier = {
    if (this.i != j)
      throw VerificationError(autoColor(i, "!=", j))

    this
  }

  def greaterThan(j: Int): IntVerifier = {
    if (this.i > j) {
      this
    } else {
      throw VerificationError(autoColor(i, "not >", j))
    }
  }

  def lessThan(j: Int): IntVerifier = {
    if (this.i < j) {
      this
    } else {
      throw VerificationError(autoColor(i, "not <", j))
    }
  }
}
