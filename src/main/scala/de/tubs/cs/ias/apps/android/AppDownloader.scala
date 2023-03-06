package de.tubs.cs.ias.apps.android

import de.tubs.cs.ias.util.AndroidConfig
import wvlet.log.LogSupport

import java.io.File
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.sys.process._
import scala.util.Random

object AppDownloader extends LogSupport {

  sealed trait GooglePlayResult

  sealed trait Panic extends GooglePlayResult {
    val msg: String
    val context: String

    def getMessage: String = s"{$context}" + msg.replace("panic:", "")
  }

  case class BadRequest(override val msg: String, override val context: String)
      extends Panic

  case class IncompatibleDevice(override val msg: String,
                                override val context: String)
      extends Panic

  case class TooManyRequests(override val context: String) extends Panic {
    override val msg: String = "too many requests"
  }

  case class PurchaseRequired(override val context: String) extends Panic {
    override val msg = "purchase required"
  }

  case class UnknownPanic(override val msg: String,
                          override val context: String)
      extends Panic {
    override def getMessage: String = "{UNKNOWN}" + super.getMessage
  }

  object Panic {

    def getPanic(panicLines: ListBuffer[String], context: String): Panic = {
      val lastLine = panicLines.filter(_.contains("panic:"))
      if (lastLine.exists(_.contains("400 Bad Request"))) {
        BadRequest(lastLine.mkString("\n"), context)
      } else if (lastLine.exists(_.contains("your device isn't compatible"))) {
        IncompatibleDevice(lastLine.mkString("\n"), context)
      } else if (lastLine.exists(_.contains("purchase required"))) {
        PurchaseRequired(context)
      } else {
        UnknownPanic(panicLines.mkString("\n"), context)
      }
    }

  }

  case class Result(value: String) extends GooglePlayResult

  object Success extends GooglePlayResult

  val DEVICE_TYPE = 1 //this is armeabi-v7a and works on our GALAXY A13
  val DELAY_TIME_MAX: Long = 10000

  /**
    * try to call the googleplay exec
    *
    * @param googleplay path to the googleplay exec
    */
  def readinessCheckGooglePlayTool(googleplay: String): Unit =
    s"$googleplay".!!

  def getAppVersion(app: String, googleplay: String): GooglePlayResult = {
    val lastLine: ListBuffer[String] = ListBuffer()
    try {
      val appInfo = s"$googleplay -p $DEVICE_TYPE -a $app"
        .!!(ProcessLogger(_ => (), err => lastLine.append(err)))
        .split("\n")
      val panicLine = lastLine.filter(_.contains("panic"))
      if (panicLine.isEmpty) {
        if (appInfo.length == 9) {
          Result(appInfo.apply(4).split(": ").apply(1))
        } else {
          UnknownPanic(
            s"unexpected output line count:\n ${lastLine.mkString("\n")}",
            "app version")
        }
      } else {
        Panic.getPanic(panicLine, "app version")
      }
    } catch {
      case _: Throwable =>
        Panic.getPanic(lastLine, "app version")
    }
  }

  def purchase(app: String, googleplay: String): GooglePlayResult = {
    val lines = ListBuffer[String]()
    try {
      val ret = s"$googleplay -purchase -p $DEVICE_TYPE -a $app" ! ProcessLogger(
        _ => (),
        err => lines.append(err))
      val panic = lines.filter(_.contains("panic:"))
      if (ret != 0 || panic.nonEmpty) {
        Panic.getPanic(lines, "purchase")
      } else {
        Success
      }
    } catch {
      case _: Throwable =>
        Panic.getPanic(lines, "purchase")
    }
  }

  def downloadApk(app: String,
                  version: String,
                  folder: String,
                  googleplay: String): GooglePlayResult = {
    val lines: ListBuffer[String] = ListBuffer()
    try {
      Thread.sleep(Random.nextLong(DELAY_TIME_MAX)) // waiting between 0 and DELAY_TIME_MAX to avoid too many requests
      val download = s"$googleplay -p $DEVICE_TYPE -a $app -s -v $version"
      val ret = Process(download, new File(folder)) ! ProcessLogger(
        _ => (),
        err => lines.append(err))
      val panic = lines.filter(_.contains("panic:"))
      if (ret != 0) {
        Panic.getPanic(lines, "download")
      } else if (panic.nonEmpty) {
        Panic.getPanic(panic, "download")
      } else {
        Success
      }
    } catch {
      case _: Throwable =>
        val panic = lines.filter(_.contains("panic:"))
        if (panic.nonEmpty) {
          Panic.getPanic(panic, "download")
        } else {
          Panic.getPanic(lines, "download")
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
      case Result(version) =>
        purchase(app, config.googleplay) match {
          case x: Panic => x
          case Result(_) =>
            throw new RuntimeException("purchase does not return a result")
          case Success =>
            downloadApk(app, version, folder, config.googleplay) match {
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
