package de.tubs.cs.ias

import de.halcony.argparse.{Parser, ParsingResult}
import de.tubs.cs.ias.Apmi.{ensureFolderStructure, getCurrentAppChartState, getTargetAppIdsFromPargs}
import de.tubs.cs.ias.OperatingSystems.ANDROID
import de.tubs.cs.ias.applist.{AppListAction, MobileAppList}
import de.tubs.cs.ias.apps.AppDownloadAction
import de.tubs.cs.ias.apps.android.AppDownloader
import de.tubs.cs.ias.labels.LabelAction
import de.tubs.cs.ias.util.ActionReportJsonWriter.actionReportCollection
import de.tubs.cs.ias.util.{ActionReport, ActionReportCollection, Config, FileSystemInteraction => fsi}
import spray.json.enrichAny
import wvlet.log.LogSupport

import scala.annotation.nowarn

object Android extends LogSupport {

  val parser : Parser = Parser("android", "perform actions for android")
    .addSubparser(Parser("download", "download app lists, privacy labels, and ipas")
      .addFlag("continue", "c", "continue", "if set the latest download will be resumed/restarted")
      .addSubparser(Parser("chart-list", "download the current chart lists only")
        .addDefault[(ParsingResult, Config) => Unit]("func", this.downloadChartListMain))
      .addSubparser(Parser("apps", "download a given list of apps")
        .addSubparser(Parser("manual", "manually specify download parameter")
          .addPositional("folder", "the folder into which to download")
          .addPositional("list", "if a file then it is assume dto be a chart list otherwise it is assume dto be a csv")
          .addDefault[(ParsingResult, Config) => Unit]("func", this.downloadAppsManual))
          .addSubparser(Parser("manual-via-txt", "manually specify download parameter")
            .addPositional("folder", "the folder into which to download")
            .addPositional("list", "a txt file containing app ids")
            .addDefault[(ParsingResult, Config) => Unit]("func", this.downloadAppsManualAppIds))
        .addSubparser(Parser("automated", "leverage the config for downloading of the apps")
          .addDefault[(ParsingResult, Config) => Unit]("func", this.downloadAppsAutomatedMain)))
      .addSubparser(Parser("labels", "download a given list of labels")
        .addSubparser(Parser("manual", "manually specify download parameter")
          .addPositional("folder", "the name of the category")
          .addPositional("list", "if a file then it is assumed to be a chart list otherwise it is assumed to be a csv")
          .addDefault[(ParsingResult, Config) => Unit]("func", this.downloadLabelsManualMain))
        .addSubparser(Parser("automated", "leverage the config for downloading of the labels")
          .addDefault[(ParsingResult, Config) => Unit]("func", this.downloadLabelsAutomatedMain)))
      .addSubparser(Parser("full-chain", "do everything from getting the current top list, downloading labels, downloading apps")
        .addDefault[(ParsingResult, Config) => Unit]("func", this.downloadTopAppsMain)))

  /** download the current chart lists and store them in the configured folder
   *
   * @param pargs the parsed command line argumetns
   * @param conf the configuration
   */
  private def downloadChartListMain(pargs : ParsingResult, conf : Config) : ActionReport = {
    val (_, listsFolder, _, _) = ensureFolderStructure(conf.downloadFolderRoot + conf.android.osFolderName + "/", pargs.getValue[Boolean]("continue"))
    info("starting chart list acquisition")
    val (appLists, report): (Map[String, MobileAppList], ActionReport) = AppListAction.download(ANDROID, conf.android.categories, listsFolder)
    appLists.foreach {
      case (category, list) =>
        info(s"category $category contains ${list.apps.length}")
    }
    info(s"overall we have ${appLists.flatMap(_._2.apps).toSet.size} unique apps")
    report
  }

  private def downloadAppsManual(pargs : ParsingResult, conf : Config) : ActionReport = {
    AppDownloader.readinessCheckGooglePlayTool(conf.android.googleplay)
    info("starting manual app acquisition")
    val downloadFolder = fsi.ensureFolderExists(pargs.getValue[String]("folder"))
    val ids : List[String] = getTargetAppIdsFromPargs(pargs.getValue[String]("list"))
    val report = AppDownloadAction.download(ids,downloadFolder,conf,ANDROID)
    report.fails.foreach {
      case (app,err) => error(s"$app : $err")
    }
    report
  }

  private def downloadAppsManualAppIds(pargs: ParsingResult, conf: Config): ActionReport = {
    AppDownloader.readinessCheckGooglePlayTool(conf.android.googleplay)
    info("starting manual app acquisition")
    val downloadFolder = fsi.ensureFolderExists(pargs.getValue[String]("folder"))

    val ids: List[String] = fsi.readInTextFile(pargs.getValue[String]("list")).split("\n").map(_.stripSuffix(".apk")).toList
    val report = AppDownloadAction.download(ids, downloadFolder, conf, ANDROID)
    report.fails.foreach {
      case (app, err) => error(s"$app : $err")
    }
    report
  }

  private def downloadAppsAutomatedMain(pargs : ParsingResult, conf : Config) : ActionReport= {
    AppDownloader.readinessCheckGooglePlayTool(conf.android.googleplay)
    val (_, _, _, appsFolder) = ensureFolderStructure(conf.downloadFolderRoot + conf.android.osFolderName + "/", pargs.getValue[Boolean]("continue"))
    info("starting automated app acquisition")
    val charts = conf.android.categories.map(_.name)
    // we assert that charts for the current iteration have already been downloaded and are in /lists/
    // this also holds for previous collection runs
    val currentAppChartList = Apmi.getCurrentAppChartState(charts, conf.downloadFolderRoot + conf.android.osFolderName + "/")
    val allAppIds = currentAppChartList.flatMap(_._2.apps.map(_.bundleId)).toList
    info(s"we have ${allAppIds.length} apps to download")
    AppDownloadAction.download(
      allAppIds.slice(0, List(allAppIds.length, conf.maxAmountApps).min),
      appsFolder,
      conf,
      ANDROID)
  }

  private def downloadLabelsManualMain(@nowarn pargs : ParsingResult,@nowarn conf : Config) : Unit = {
    info("starting manual label acquisition")
    val targetFolder = fsi.ensureFolderExists(pargs.getValue[String]("folder") + "/")
    val labelIds = getTargetAppIdsFromPargs(pargs.getValue[String]("list"))
    LabelAction.download(labelIds, targetFolder, ANDROID)
  }

  private def downloadLabelsAutomatedMain(@nowarn pargs: ParsingResult, @nowarn conf: Config): ActionReport  = {
    val (_, _, labelsFolder, _) = ensureFolderStructure(conf.downloadFolderRoot + conf.android.osFolderName + "/", pargs.getValue[Boolean]("continue"))
    info("starting automated label acquisition")
    val charts = conf.android.categories.map(_.name)
    val currentAppChartList = getCurrentAppChartState(charts, conf.downloadFolderRoot + conf.android.osFolderName + "/")
    val allAppIds = currentAppChartList.flatMap(_._2.apps.map(_.bundleId)).toList
    info(s"we have ${allAppIds.length} labels to download")
    LabelAction.download(allAppIds.slice(0, List(allAppIds.length, conf.maxAmountApps).min), labelsFolder, ANDROID)
  }

  private def downloadTopAppsMain(pargs : ParsingResult, conf : Config) : Unit = {
    try {
      info("running full chain download")
      val (baseFolder, _, _, _) = ensureFolderStructure(conf.downloadFolderRoot + conf.android.osFolderName + "/", pargs.getValue[Boolean]("continue"))
      val chartReport: ActionReport = this.downloadChartListMain(pargs, conf)
      val labelsReport: ActionReport = this.downloadLabelsAutomatedMain(pargs, conf)
      val appsReport: ActionReport = this.downloadAppsAutomatedMain(pargs, conf)
      fsi.writeFile(ActionReportCollection(
        Map(
          "charts" -> chartReport,
          "labels" -> labelsReport,
          "apps" -> appsReport
        )
      ).toJson.prettyPrint, s"$baseFolder/run.json")
    } catch {
      case e: Error =>
        error(e.getMessage)
      case e: Exception =>
        error(e.getMessage)
    }
  }




}
