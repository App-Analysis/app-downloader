package de.tubs.cs.ias.util

import java.io.{File, FileWriter}
import scala.io.Source
import sys.process._

object FileSystemInteraction {

  def fileExists(path: String): Boolean = {
    new File(path).exists()
  }

  def readInTextFile(path: String): String = {
    val source = Source.fromFile(path)
    try {
      source.getLines().mkString("\n")
    } finally {
      source.close()
    }
  }

  def ensureFolderExists(path: String): String = {
    assert(path.charAt(path.length - 1) == '/', "the path has to end on a /")
    if (!new File(path).exists()) {
      new File(path).mkdirs()
    }
    path
  }

  def writeFile(contents: String, path: String): Unit = {
    val fw = new FileWriter(new File(path))
    try {
      fw.write(contents)
    } finally {
      fw.close()
    }
  }

  def moveFile(from: String, to: String): Unit = {
    s"mv $from $to".!!
  }

  def getSubfolder(path: String): List[String] = {
    assert(path.charAt(path.length - 1) == '/', "the path has to end on a /")
    new File(path).listFiles.filter(_.isDirectory).map(_.getAbsolutePath).toList
  }

  def getFiles(path: String, suffix: Option[String] = None): List[String] = {
    suffix match {
      case Some(value) =>
        new File(path).listFiles
          .filter(_.isFile)
          .filter(_.getName.endsWith(value))
          .map(_.getName)
          .toList
      case None =>
        new File(path).listFiles.filter(_.isFile).map(_.getName).toList
    }
  }
}
