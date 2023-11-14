package de.tubs.cs.ias

import de.halcony.argparse.{Parser, ParsingResult}
import de.tubs.cs.ias.applist.AppListParser.appListFormat
import de.tubs.cs.ias.applist.MobileAppList
import de.tubs.cs.ias.util.{Config, FileSystemInteraction => fsi}
import spray.json.{JsNull, JsNumber, JsObject, JsonParser}
import wvlet.log.LogSupport

import java.io.File

object Analysis extends LogSupport {

  val parser: Parser = Parser("analysis", "analyze the downloaded Apps")
    .addPositional("os", "select the OS to analyze {Android,iOS}")
    .addOptional("folder",
                 "f",
                 "folder",
                 None,
                 "if provided overwrites the folder of the config")
    .addDefault[(ParsingResult, Config) => Unit]("func", analysisMain)

  /**
    *
    * {
    *    <DATE> : {
    *      listCount : <Count>,
    *      labelCount : <Count>,
    *      appCount : <Count>,
    *      individual {
    *          listCount : <Count>,
    *          labelCount : <Count>,
    *          appCount : <Count>
    *      }
    *    },
    *    ...
    * }
    *
    * @param pargs parsed command line arguments
    * @param config the configuration file
    */
  private def analysisMain(pargs: ParsingResult, config: Config): Unit = {
    val baseFolder = pargs.getValue[String]("os").toLowerCase match {
      case "android" =>
        pargs.getValueOrElse[String]("folder", config.downloadFolderRoot + "/" + config.android.osFolderName)
      case "ios" => ???
    }
    info(
      s"analyzing the download results for ${pargs.getValue[String]("os")} located at $baseFolder")
    val individual = new File(baseFolder)
      .listFiles(_.isDirectory)
      .map { folder =>
        Analysis(folder.getAbsolutePath,
                 pargs.getValue[String]("os").toLowerCase)
      }
      .sortBy(_.date) //assuming smallest to largest date sorting
    val timeline : List[Analysis] = if (individual.length > 1) {
      var last = individual.head
      println(individual.map(_.date).mkString(","))
      List(individual.head) ++ individual.tail.map { indi =>
        val buff = indi.addPrevious(last)
        last = buff
        buff
      }.toList
    } else {
      individual.toList
    }
    println(
      JsObject(
        timeline.map(elem => elem.date -> elem.getJson).toMap
      ).prettyPrint)
  }

  def apply(folder: String, os: String): Analysis = {
    info(folder)
    val date = folder.split('/').last
    info(s"reading in results for downloads on the $date")
    val list = new File(folder + "/lists/")
      .listFiles(file => file.isFile && file.getName.endsWith(".json"))
      .map(file =>
        JsonParser(fsi.readInTextFile(file.getAbsolutePath))
          .convertTo[MobileAppList])
      .flatMap(_.apps.map(_.bundleId))
      .toSet
    val labels = new File(folder + "/labels/")
      .listFiles(file => file.isFile && file.getName.endsWith(".json"))
      .map(file => file.getName.split('.').reverse.tail.reverse.mkString("."))
      .toSet
    val apps = if (os == "android") {
      new File(folder + "/apps/")
        .listFiles(file => file.isFile && file.getName.endsWith(".apk"))
        .map(file => file.getName.split('-').reverse.tail.reverse.mkString("-"))
        .toSet
    } else {
      ???
    }
    Analysis(date, list, labels, apps)
  }

}

case class Analysis(date: String,
                    list: Set[String],
                    labels: Set[String],
                    apps: Set[String],
                    previousList: Option[Set[String]] = None) {

  def getJson: JsObject = {
    JsObject(
      "1-listCount" -> JsNumber(getFullListPotential.size),
      "2-labelsCount" -> JsNumber(labels.size),
      "3-appCount" -> JsNumber(apps.size),
      "4-previous" -> (previousList match {
        case Some(value) => JsNumber(value.size)
        case None        => JsNull
      }),
      "5-individual" -> JsObject(
        "1-listCount" -> JsNumber(list.size),
        "2-labelsCount" -> JsNumber(labels.intersect(list).size),
        "3-appCount" -> JsNumber(apps.intersect(list).size)
      )
    )
  }

  def getFullListPotential: Set[String] =
    list ++ previousList.getOrElse(Set[String]())

  def addPrevious(previous: Analysis): Analysis = {
    assert(date > previous.date,
           "the added previous download has to be from an earlier time")
    Analysis(date,
             list,
             labels,
             apps,
             Some(getFullListPotential ++ previous.getFullListPotential))
  }

}
