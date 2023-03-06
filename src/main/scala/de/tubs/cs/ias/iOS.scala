package de.tubs.cs.ias

import de.halcony.argparse.{Parser, ParsingResult}
import de.tubs.cs.ias.Apmi.{ensureFolderStructure, getCurrentAppChartState, getTargetAppIdsFromPargs}
import de.tubs.cs.ias.OperatingSystems.IOS
import de.tubs.cs.ias.applist.{AppListAction, MobileAppList}
import de.tubs.cs.ias.apps.AppDownloadAction
import de.tubs.cs.ias.labels.LabelAction
import de.tubs.cs.ias.util.ActionReportJsonWriter.actionReportCollection
import de.tubs.cs.ias.util.{ActionReport, ActionReportCollection, Config, FileSystemInteraction => fsi}
import spray.json.enrichAny
import wvlet.log.LogSupport

import scala.annotation.nowarn


object iOS extends LogSupport {

  val parser : Parser = Parser("ios","perform actions for ios")
    .addSubparser(Parser("download", "download app lists, privacy labels, and ipas")
      .addFlag("continue", "c", "continue", "if set the latest download will be resumed/restarted")
      .addSubparser(Parser("chart-list", "download the current chart lists only")
        .addDefault[(ParsingResult, Config) => Unit]("func", downloadChartListMain))
      .addSubparser(Parser("apps", "download a given list of apps")
        .addSubparser(Parser("manual", "manually specify download parameter")
          .addPositional("folder", "the folder into which to download")
          .addPositional("list", "if a file then it is assume dto be a chart list otherwise it is assume dto be a csv")
          .addDefault[(ParsingResult, Config) => Unit]("func", downloadAppsManual))
        .addSubparser(Parser("automated", "leverage the config for downloading of the apps")
          .addDefault[(ParsingResult, Config) => Unit]("func", downloadAppsAutomatedMain)))
      .addSubparser(Parser("labels", "download a given list of labels")
        .addSubparser(Parser("manual", "manually specify download parameter")
          .addPositional("folder", "the name of the category")
          .addPositional("list", "if a file then it is assumed to be a chart list otherwise it is assumed to be a csv")
          .addDefault[(ParsingResult, Config) => Unit]("func", downloadLabelsManualMain))
        .addSubparser(Parser("automated", "leverage the config for downloading of the labels")
          .addDefault[(ParsingResult, Config) => Unit]("func", downloadLabelsAutomatedMain)))
      .addSubparser(Parser("full-chain", "do everything from getting the current top list, downloading labels, downloading apps")
        .addDefault[(ParsingResult, Config) => Unit]("func", downloadTopAppsMain)))


  /** download the apple app chart list
   *
   * @param pargs the parsed command line arguments
   * @param conf the configuration
   * @return an action report
   */
  private def downloadChartListMain(pargs: ParsingResult, conf: Config): ActionReport = {
    val (_, listsFolder, _, _) = ensureFolderStructure(conf.downloadFolderRoot  + conf.ios.osFolderName + "/", pargs.getValue[Boolean]("continue"))
    info("starting chart list acquisition")
    val (appLists, report): (Map[String, MobileAppList], ActionReport) = AppListAction.download(IOS,conf.ios.categories, listsFolder)
    appLists.foreach {
      case (category, list) =>
        info(s"category $category contains ${list.apps.length}")
    }
    info(s"overall we have ${appLists.flatMap(_._2.apps).toSet.size} unique apps")
    report
  }

  /** download all labels of currently referenced apps within the donwload folder context
   *
   * @param pargs the parsed command line arguments
   * @param conf the parsed configuration
   * @return a report on the success and failure of the action
   */
  private def downloadLabelsAutomatedMain(pargs: ParsingResult, conf: Config): ActionReport = {
    val (_, _, labelsFolder, _) = ensureFolderStructure(conf.downloadFolderRoot  + conf.ios.osFolderName + "/", pargs.getValue[Boolean]("continue"))
    info("starting automated labels acquisition")
    val charts = conf.ios.categories.map(_.name)
    // we assert that charts for the current iteration have already been downloaded and are in /lists/
    // this also holds for previous collection runs
    val currentAppChartList = getCurrentAppChartState(charts, conf.downloadFolderRoot  + conf.ios.osFolderName + "/")
    val allAppIds = currentAppChartList.flatMap(_._2.apps.map(_.bundleId)).toList
    info(s"we have ${allAppIds.length} labels to download")
    LabelAction.download(allAppIds.slice(0, List(allAppIds.length, conf.maxAmountApps).min), labelsFolder, IOS)
  }

  /** download the specified labels within the cmd arguments
   *
   * @param pargs the parsed command line arguments
   * @param conf the parsed configuration
   */
  private def downloadLabelsManualMain(pargs: ParsingResult, @nowarn conf: Config): Unit = {
    info("starting manual label acquisition")
    val targetFolder = fsi.ensureFolderExists(pargs.getValue[String]("folder") + "/")
    val labelIds = getTargetAppIdsFromPargs(pargs.getValue[String]("list"))
    LabelAction.download(labelIds, targetFolder, IOS)
  }

  private def downloadAppsAutomatedMain(pargs: ParsingResult, conf: Config): ActionReport = {
    val (_, _, _, appsFolder) = ensureFolderStructure(conf.downloadFolderRoot  + conf.ios.osFolderName + "/", pargs.getValue[Boolean]("continue"))
    info("starting automated app acquisition")
    val charts = conf.ios.categories.map(_.name)
    // we assert that charts for the current iteration have already been downloaded and are in /lists/
    // this also holds for previous collection runs
    val currentAppChartList = getCurrentAppChartState(charts, conf.downloadFolderRoot  + conf.ios.osFolderName + "/")
    val allAppIds = currentAppChartList.flatMap(_._2.apps.map(_.bundleId)).toList
    info(s"we have ${allAppIds.length} apps to download")
    AppDownloadAction.download(allAppIds.slice(0, List(allAppIds.length, conf.maxAmountApps).min), appsFolder, conf, IOS)
  }

  private def downloadAppsManual(pargs: ParsingResult, conf: Config): Unit = {
    info("starting manual app acquisition")
    val downloadFolder = fsi.ensureFolderExists(pargs.getValue[String]("folder"))
    val ids: List[String] = getTargetAppIdsFromPargs(pargs.getValue[String]("list"))
    AppDownloadAction.download(ids, downloadFolder, conf,IOS)
  }

  private def downloadTopAppsMain(pargs: ParsingResult, conf: Config): Unit = {
    try {
      val (baseFolder, _, _, _) = ensureFolderStructure(conf.downloadFolderRoot + conf.ios.osFolderName + "/", pargs.getValue[Boolean]("continue"))
      val chartReport = downloadChartListMain(pargs, conf)
      val labelsReport = downloadLabelsAutomatedMain(pargs, conf)
      val appsReport = downloadAppsAutomatedMain(pargs, conf)
      fsi.writeFile(ActionReportCollection(
        Map(
          "charts" -> chartReport,
          "labels" -> labelsReport,
          "apps" -> appsReport
        )
      ).toJson.prettyPrint, s"$baseFolder/run.json")
    } catch {
      case e: Error =>
      error(e)
      case e: Exception =>
        error(e)
    } finally {

    }
  }

}
