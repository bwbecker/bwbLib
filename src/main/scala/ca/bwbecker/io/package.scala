package ca.bwbecker

import scala.util.{Try}

import java.io.File

package object io {

  /**
    * Usage:
    * import java.io._
    * val data = Array("Five","strings","in","a","file!")
    * printToFile(new File("example.txt")) { p =>
    *data.foreach(p.println)
    * }
    */
  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try {op(p)} finally {p.close()}
  }


  /**
    * Return the entire contents of a file as a string.
    */
  def readAsString(f: java.io.File): Try[String] = {
    Try {
      val source = scala.io.Source.fromFile(f)
      val rawJson = try source.getLines().mkString("\n") finally source.close()
      rawJson
    }
  }

  def readAsString(fileName: String): Try[String] = {
    this.readAsString(new File(fileName))
  }
}
