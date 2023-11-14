package de.tubs.cs.ias

import de.halcony.argparse.{Parser, ParsingException, ParsingResult}
import de.tubs.cs.ias.applist.{AppListAction, MobileAppList}
import de.tubs.cs.ias.download.Downloader
import de.tubs.cs.ias.util.{Config, FileSystemInteraction => fsi}
import wvlet.log.LogFormatter.PlainSourceCodeLogFormatter
import wvlet.log.{FileHandler, LogSupport}

import java.io.File
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.annotation.nowarn

object Apmi extends LogSupport {

  private val parser =
    Parser("Apmi", "tool to keep up to date with the current state of iOS apps")
      .addOptional("config",
                   "c",
                   "config",
                   Some("./config.json"),
                   "the config file used for running")
      .addSubparser(iOS.parser)
      .addSubparser(Android.parser)
      .addSubparser(Downloader.parser)
      .addSubparser(Analysis.parser)
      .addSubparser(
        Parser("sentinel", "perform a sanity check on a given download folder")
          .addDefault[(ParsingResult, Config) => Unit]("func", sentinelMain))

  def main(args: Array[String]): Unit = {
    try {
      val pargs = parser.parse(args)
      val config = Config.read(pargs.getValue[String]("config"))
      try {
        if (config.telegram.enable) {}
        pargs
          .getValue[(ParsingResult, Config) => Unit]("func")
          .apply(pargs, config)
      } finally {
        if (config.telegram.enable) {}
      }
    } catch {
      case _: ParsingException =>
    }
  }

  case class RunFolderStructure(mainFolder: String,
                                listsFolder: String,
                                labelsFolder: String,
                                appsFolder: String) {
    def getFolderStructure: (String, String, String, String) =
      (mainFolder, listsFolder, labelsFolder, appsFolder)

    override def toString: String =
      s"main:$mainFolder, lists:$listsFolder, labels:$labelsFolder, apps:$appsFolder"
  }

  var folderStructure: Option[RunFolderStructure] = None

  def ensureFolderStructure(
      downloadFolderRoot: String,
      continue: Boolean): (String, String, String, String) = {
    if (continue) {
      ensureFolderStructureContinue(downloadFolderRoot)
    } else {
      ensureFolderStructureCreate(downloadFolderRoot)
    }
  }

  private def ensureFolderStructureContinue(
      downloadFolderRoot: String): (String, String, String, String) = {
    folderStructure match {
      case Some(value) => value.getFolderStructure
      case None =>
        assert(downloadFolderRoot.charAt(downloadFolderRoot.length - 1) == '/',
               "the path has to end on a /")
        val timeFolder = fsi
          .getSubfolder(downloadFolderRoot)
          .map { path =>
            assert(path.charAt(path.length - 1) != '/',
                   "we assume that the last element of the path is not /")
            path.split("/").reverse.head
          }
          .max
        val currentMainFolder = s"$downloadFolderRoot/$timeFolder/"
        val listsFolder = s"$downloadFolderRoot/$timeFolder/lists/"
        val labelsFolder = s"$downloadFolderRoot/$timeFolder/labels/"
        val appsFolder = s"$downloadFolderRoot/$timeFolder/apps/"
        wvlet.log.Logger.rootLogger.resetHandler(
          new FileHandler(
            fileName = s"$currentMainFolder/report.log",
            formatter = PlainSourceCodeLogFormatter
          )
        )
        folderStructure = Some(
          RunFolderStructure(currentMainFolder,
                             listsFolder,
                             labelsFolder,
                             appsFolder))
        folderStructure.get.getFolderStructure
    }
  }

  private def ensureFolderStructureCreate(
      downloadFolderRoot: String): (String, String, String, String) = {
    folderStructure match {
      case Some(value) => value.getFolderStructure
      case None =>
        assert(downloadFolderRoot.charAt(downloadFolderRoot.length - 1) == '/',
               "the path has to end on a /")
        val currentDay: String =
          LocalDateTime.now.format(DateTimeFormatter.ofPattern("yyyyMMdd"))
        val currentMainFolder =
          fsi.ensureFolderExists(s"$downloadFolderRoot/$currentDay/")
        val listsFolder =
          fsi.ensureFolderExists(s"$downloadFolderRoot/$currentDay/lists/")
        val labelsFolder =
          fsi.ensureFolderExists(s"$downloadFolderRoot/$currentDay/labels/")
        val appsFolder =
          fsi.ensureFolderExists(s"$downloadFolderRoot/$currentDay/apps/")
        wvlet.log.Logger.rootLogger.resetHandler(
          new FileHandler(
            fileName = s"$currentMainFolder/report.log",
            formatter = PlainSourceCodeLogFormatter
          )
        )
        folderStructure = Some(
          RunFolderStructure(currentMainFolder,
                             listsFolder,
                             labelsFolder,
                             appsFolder))
        folderStructure.get.getFolderStructure
    }
  }

  def getCurrentAppChartState(
      monitoredCharts: List[String],
      baseFolder: String): Map[String, MobileAppList] = {
    info(
      s"there are ${fsi.getSubfolder(baseFolder).length} collection cycles present")
    val ret = monitoredCharts.map { chartName =>
      val overTime = fsi
        .getSubfolder(baseFolder)
        .map { timeFolder =>
          val expectedListPath = timeFolder + s"/lists/$chartName.json"
          if (fsi.fileExists(expectedListPath)) {
            AppListAction.readInAppList(expectedListPath)
          } else {
            warn(s"we did not collect $chartName list in $timeFolder")
            MobileAppList(List(), "N/A")
          }
        }
        .filterNot(_.apps.isEmpty)
      info(
        s"we have ${overTime.length} previous collection cycles for $chartName")
      if (overTime.nonEmpty) {
        val buff = overTime.reduce((lhs, rhs) => {
          assert(lhs.name == rhs.name)
          MobileAppList(lhs.apps ++ rhs.apps, rhs.name)
        })
        chartName -> MobileAppList(buff.apps.distinct, buff.name)
      } else {
        chartName -> MobileAppList(List(), "N/A")
      }
    }.toMap
    ret.foreach {
      case (key, value) =>
        if (value.apps.isEmpty) {
          warn(s"we do not have previous apps for $key")
        }
    }
    ret
  }

  @nowarn
  private def getFinishedElements(folder: String,
                                  suffix: String): Set[String] = {
    fsi
      .getFiles(folder, Some(suffix))
      .map { file =>
        file.split("\\.").head
      }
      .toSet
  }

  def getTargetAppIdsFromPargs(list: String): List[String] = {
    if (new File(list).exists()) {
      info("using given chart list file")
      AppListAction.readInAppList(list).apps.map(elem => elem.bundleId).distinct
    } else {
      info("using given csv values")
      list.split(",").toList
    }
  }

  private def sentinelMain(@nowarn pargs: ParsingResult,
                           @nowarn conf: Config): Unit = {
    throw new NotImplementedError()
  }
}
