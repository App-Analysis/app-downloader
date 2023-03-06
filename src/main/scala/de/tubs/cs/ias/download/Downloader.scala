package de.tubs.cs.ias.download

import de.halcony.argparse.{OptionalValue, Parser, ParsingResult}
import de.tubs.cs.ias.applist.{AppListAction, MobileApp}
import de.tubs.cs.ias.util.Config
import wvlet.log.LogSupport

import java.io.File
import scala.collection.mutable.ListBuffer
import scala.sys.process.{ProcessLogger, _}

object Downloader extends LogSupport {

  val parser: Parser = Parser("download",
                              "download apps,labels,files acquired via appimo")
    .addPositional("destination",
                   "the base folder where to put the downloaded stuff")
    .addPositional("os", "the operating system for which to download the data")
    .addPositional("date", "the date of interest")
    .addSubparser(Parser("lists", "download the configured lists")
      .addDefault[(ParsingResult, Config) => Unit]("func", downloadLists))
    .addSubparser(
      Parser("apps", "download apps of relevance")
        .addOptional("types",
                     "t",
                     "category-types",
                     None,
                     "which categories to download")
        .addOptional(
          "rank",
          "r",
          "rank",
          None,
          "what the minimum rank to download is (only apps and labels)")
        .addDefault[(ParsingResult, Config) => Unit]("func", downloadApps))

  private def scp(server: String,
                  source: String,
                  dest: String): (Boolean, String) = {
    val cmd = s"scp $server:$source $dest"
    try {
      val stderr = new ListBuffer[String]()
      val ret = cmd ! ProcessLogger(_ => (), line => stderr.append(line))
      if (ret == 0 && stderr.isEmpty) {
        (true, "")
      } else {
        (false, stderr.mkString("\n"))
      }
    } catch {
      case x: Throwable =>
        (false, x.getMessage)
    }
  }

  private def parseDate(date: String): String = {
    if (date.contains("-")) {
      assert(date.length == 10)
      s"${date.slice(0, 4)}${date.slice(5, 7)}${date.slice(8, 10)}"
    } else {
      assert(date.length == 8)
      date
    }
  }

  def downloadLists(pargs: ParsingResult, conf: Config): Unit = {
    val downloaderConfig = DownloaderConfig(conf.downloaderConfig)
    val date = parseDate(pargs.getValue[String]("date"))
    val destination = new File(
      pargs.getValue[String]("destination") + "/lists/")
    if (destination.exists()) {
      assert(destination.isDirectory)
    } else {
      destination.mkdirs()
    }
    val folder = pargs.getValue[String]("os") match {
      case "android" => downloaderConfig.remoteAndroid
      case "ios"     => downloaderConfig.remoteIos
    }
    info(
      s"downloading all lists via scp ${downloaderConfig.sshString}:$folder/$date/lists/* into ${destination.getAbsolutePath}/")
    scp(downloaderConfig.sshString,
        s"$folder/$date/lists/*",
        destination.getAbsolutePath + "/") match {
      case (true, _)    => info("success")
      case (false, msg) => error(msg)
    }
  }

  private def readInAvailableApps(file: String): List[MobileApp] = {
    new File(file + "/lists/")
      .listFiles()
      .filter(_.isFile)
      .filter(_.getName.endsWith(".json"))
      .map(file => AppListAction.readInAppList(file.getPath))
      .flatMap(_.apps)
      .toList
  }

  def downloadApps(pargs: ParsingResult, conf: Config): Unit = {
    val downloaderConfig = DownloaderConfig(conf.downloaderConfig)
    val date = parseDate(pargs.getValue[String]("date"))
    val destination = pargs.getValue[String]("destination")
    if (new File(destination + "/apps/").exists()) {
      assert(new File(destination + "/apps/").isDirectory)
    } else {
      new File(destination + "/apps/").mkdirs()
    }
    val apps = readInAvailableApps(destination)
    info(s"we have a selection ${apps.length} apps")
    val minRank = pargs.get[OptionalValue[String]]("rank").value match {
      case Some(value) => Some(value.toInt)
      case None        => None
    }
    val categories: Set[String] =
      pargs.get[OptionalValue[String]]("types").value match {
        case Some(value) => value.split(",").toSet
        case None        => Set()
      }
    val folder = (pargs.getValue[String]("os") match {
      case "android" => downloaderConfig.remoteAndroid
      case "ios"     => downloaderConfig.remoteIos
    }) + s"$date/apps/"
    var succ = 0
    var want = 0
    apps.foreach { app =>
      if ((categories.isEmpty || categories.contains(app.category)) &&
          (minRank.isEmpty || minRank.get >= app.rank)) {
        want = want + 1
        val (sourceFile, destFile) = pargs.getValue[String]("os") match {
          case "android" =>
            (folder + app.bundleId + "*.apk",
             s"$destination/apps/${app.bundleId}.apk")
          case "ios" =>
            (app.bundleId + ".ipa", s"$destination/apps/${app.bundleId}.ipa")
        }
        if (new File(destFile).exists()) {
          succ = succ + 1
        } else {
          scp(downloaderConfig.sshString, sourceFile, destFile) match {
            case (true, _) =>
              info(s"downloaded ${app.bundleId}")
              succ = succ + 1
            case (false, msg) =>
              error(s"download of ${app.bundleId} failed: $msg")
          }
        }
      } else {
        //info(s"app ${app.bundleId} does not match requirements")
      }
    }
    info(s"we were able to acquire $succ/$want apps")
  }

}
