package ca.bwbecker

package object io {

  /**
  Usage:
  import java.io._
val data = Array("Five","strings","in","a","file!")
printToFile(new File("example.txt")) { p =>
  data.foreach(p.println)
}
    */
  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try {op(p)} finally {p.close()}
  }

}
