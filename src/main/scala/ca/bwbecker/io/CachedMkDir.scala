package ca.bwbecker.io

import scala.collection.mutable.Set

import java.io.File

/**

Usage:
 val dircache = new CachedMkdir

 dircache.mkdirp("a/b/c/d/e") // creates the directory structure
 dircache.mkdirp("a/b/c/d/e") // does nothing
 dircache.mkdirp("a/b/Z/Y/Z") // only creates from b/ down
 from https://gist.github.com/capotej/1975463
  */
class CachedMkdir {

  val cache = Set[String]()

  def mkdirp(path: String) {
    var prepath = ""

    for (dir <- path.split("/")) {
      prepath += (dir + "/")
      if (!cache.contains(prepath)) {
        new java.io.File(prepath).mkdir()
        cache += prepath
      }
    }
  }

  def mkdirp(path:File): Unit = mkdirp(path.getAbsolutePath)

}
