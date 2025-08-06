package de.tubs.cs.ias.apps.android

import de.tubs.cs.ias.util.AndroidConfig
import wvlet.log.LogSupport

import java.io.File
import java.lang
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.sys.process._
import scala.util.Random

object AppDownloader extends LogSupport {

  sealed trait GooglePlayResult

  sealed trait Panic extends GooglePlayResult {
    val cmd: String
    val msg: String
    val context: String

    def getMessage: String = s"[$cmd]{$context}" + msg.replace("panic:", "")
  }

  case class BadRequest(override val cmd: String,
                        override val msg: String,
                        override val context: String)
      extends Panic

  case class IncompatibleDevice(override val cmd: String,
                                override val msg: String,
                                override val context: String)
      extends Panic

  case class TooManyRequests(override val cmd: String,
                             override val context: String)
      extends Panic {
    override val msg: String = "too many requests"
  }

  case class PurchaseRequired(override val cmd: String,
                              override val context: String)
      extends Panic {
    override val msg = "purchase required"
  }

  case class UnknownPanic(override val cmd: String,
                          override val msg: String,
                          override val context: String)
      extends Panic {}

  object Panic {

    def getPanic(cmd: String,
                 panicLines: ListBuffer[String],
                 context: String): Panic = {
      val lastLine = panicLines.filter(_.contains("panic:"))
      if (lastLine.exists(_.contains("400 Bad Request"))) {
        BadRequest(cmd, lastLine.mkString("\n"), context)
      } else if (lastLine.exists(_.contains("your device isn't compatible"))) {
        IncompatibleDevice(cmd, lastLine.mkString("\n"), context)
      } else if (lastLine.exists(_.contains("purchase required"))) {
        PurchaseRequired(cmd, context)
      } else {
        UnknownPanic(cmd, panicLines.mkString("\n"), context)
      }
    }

  }

  case class Result(value: String) extends GooglePlayResult

  class InfoResult(output: Array[String]) extends GooglePlayResult {
    /*assert(
      output.length == 12,
      s"expected 12 line output but got ${output.length} \n ${output.mkString("\n")}")
    assert(output(0).startsWith("msg=POST"))
    assert(output(1).startsWith("msg=GET"))*/
    val vendor: String = extractValue(output(5))
    val price: String = extractValue(output(0))
    val versionName: String = extractValue(output(1))
    val lastUpdate: String = extractValue(output(2))
    val apks : String = extractValue(output(3))
    val minOs: String = extractValue(output(4))
    val downloadCount: String = extractValue(output(7))
    val name: String = extractValue(output(8)) 
    val size: String = extractValue(output(9))
    val versionCode: String = extractValue(output(10))

    private def extractValue(value: String): String = {
      value.split("=").toList match {
        case ident :: value :: Nil => value.trim
        case _ :: values => values.mkString("=").trim
        case _ =>
          throw new RuntimeException(s"unexpected key value pair $value")
      }
    }
  }

  object Success extends GooglePlayResult

  val DEVICE_TYPE = "arm64-v8a" //this is arm64-v8a and works on our Pixel 6a
  val DELAY_TIME_MAX: Long = 10000

  /**
    * try to call the googleplay exec
    *
    * @param googleplay path to the googleplay exec
    */
  def readinessCheckGooglePlayTool(googleplay: String): Unit = {
    assert(new File(googleplay).canExecute,
           "the provided google play program is not executable")
    //val ret : String = s"$googleplay".!!
  }

  def getAppVersion(app: String, googleplay: String): GooglePlayResult = {
    val lastLine: ListBuffer[String] = ListBuffer()
    val stdLine: ListBuffer[String] = ListBuffer()
    val cmd = s"$googleplay -abi \"$DEVICE_TYPE\" -i $app"
    try {
      val appInfo = cmd
        .!!(
          ProcessLogger(std => stdLine.append(std),
                        err => lastLine.append(err)))
        .split("\n")
      val panicLine = lastLine.filter(_.contains("panic"))
      if (panicLine.isEmpty) {
        try {
	    val ret = new InfoResult(appInfo)
	    ret
        } catch {
          case _: Throwable =>
            UnknownPanic(
              cmd,
              s"unexpected info output:\n ${lastLine.mkString("\n")}",
              "app version")
        }
      } else {
        Panic.getPanic(cmd, panicLine, "app version")
      }
    } catch {
      case _: Throwable =>
        Panic.getPanic(cmd, lastLine, "app version")
    }
  }

  def purchase(app: String, googleplay: String): GooglePlayResult = {
    val errLines = ListBuffer[String]()
    val stdLines = ListBuffer[String]()
    //val cmd = s"$googleplay -abi $DEVICE_TYPE -a $app"
    val cmd = s"$googleplay -abi \"$DEVICE_TYPE\" -i $app -a"
    try {
      val ret = cmd ! ProcessLogger(_ => (), err => errLines.append(err))
      val panic = errLines.filter(_.contains("panic:"))
      if (ret != 0 || panic.nonEmpty) {
        Panic.getPanic(cmd, errLines, "purchase")
      } else {
        Success
      }
    } catch {
      case _: Throwable =>
        Panic.getPanic(cmd, errLines, "purchase")
    }
  }

  def downloadApk(app: String,
                  version: String,
                  folder: String,
                  googleplay: String): GooglePlayResult = {
    val errlines: ListBuffer[String] = ListBuffer()
    val stdlines: ListBuffer[String] = ListBuffer()
    val download = s"$googleplay -abi \"$DEVICE_TYPE\" -i $app -v $version"
    try {
      Thread.sleep(Random.nextLong(DELAY_TIME_MAX)) // waiting between 0 and DELAY_TIME_MAX to avoid too many requests
      val ret = Process(download, new File(folder)) ! ProcessLogger(
        std => stdlines.append(std),
        err => errlines.append(err))
      val panic = errlines.filter(_.contains("panic:"))
      if (ret != 0) {
        Panic.getPanic(download, errlines, "download")
      } else if (panic.nonEmpty) {
        Panic.getPanic(download, panic, "download")
      } else {
        Success
      }
    } catch {
      case _: Throwable =>
        val panic = errlines.filter(_.contains("panic:"))
        if (panic.nonEmpty) {
          Panic.getPanic(download, panic, "download")
        } else {
          Panic.getPanic(download, errlines, "download")
        }
    }
  }

  @tailrec
  def download(app: String,
               folder: String,
               config: AndroidConfig,
               maxRetries: Int = 3): GooglePlayResult = {
    getAppVersion(app, config.googleplay) match {
      case x: Panic => x
      case Success =>
        throw new RuntimeException(
          "getAppVersion must not return success object")
      case result: InfoResult =>
        purchase(app, config.googleplay) match {
          case x: Panic => x
          case Result(_) =>
            throw new RuntimeException("purchase does not return a result")
          case Success =>
            downloadApk(app, result.versionCode, folder, config.googleplay) match {
              case x: BadRequest =>
                if (maxRetries == 0) {
                  x
                } else {
                  warn("encountered bad request ... retrying")
                  Thread.sleep(Random.nextLong(DELAY_TIME_MAX))
                  download(app, folder, config, maxRetries - 1)
                }
              case panic: Panic => panic
              case Result(_) =>
                throw new RuntimeException(
                  "download Apk does not return a result")
              case Success => Success
            }

        }
    }
  }

}
