package ca.bwbecker.utest

import utest.ufansi.Str


trait Verify {

  def verify(s: String) = new StringVerifier(s)

  def verify(i: Int) = new IntVerifier(i)

  def verify(i: Double) = new DoubleVerifier(i)

  def verify[T](s: Seq[T]) = new SeqVerifier[T](s)

  def verify[T](a: Array[T]) = new SeqVerifier[T](a.toSeq)


  /**
    * Do a version of scalaTest's assertResult so we don't need to reformat them all.
    * Preference is to NOT use this except for converting existing tests.
    */
  def assertResult[T](expected: T)(actual: Any): Unit = {
    utest.assert(actual == expected)
  }


  /**
    * Mark a test as pending. It's meant to be used like so:
    * "this is an unfinished test" - pending {
    *   [code]
    * }
    * This way, you don't need to comment out the code, as long as it compiles.
    * Does not evaluate and ignores its argument (testBody: ⇒ Any). */
  val pending: ( ⇒ Any) ⇒ Str = testBody ⇒ utest.ufansi.Color.Cyan("pending")


  /** Immediately fails with the provided error message.
    *
    * @param errMsg Message to throw.
    * @return Throws VerificationError.
    */
  def fail(errMsg: String) = throw ca.bwbecker.utest.VerificationError(errMsg)
}
