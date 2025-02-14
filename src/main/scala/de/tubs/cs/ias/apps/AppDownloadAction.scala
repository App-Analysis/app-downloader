package de.tubs.cs.ias.apps

import de.tubs.cs.ias.OperatingSystems.OperatingSystem
import de.tubs.cs.ias.apps.android.{AppDownloader => AndroidAppDownloader}
import de.tubs.cs.ias.labels.LabelAction.sendTelegramMessage
import de.tubs.cs.ias.util.{
  ActionReport,
  AsciiProgressBar,
  Config,
  TelegramSupport
}
import wvlet.log.LogSupport

import java.io.File
import scala.collection.mutable.{Map => MMap}

object AppDownloadAction extends LogSupport with TelegramSupport {

  def download(appIds: List[String],
               folder: String,
               conf: Config,
               os: OperatingSystem): ActionReport = {
    val bar =
      AsciiProgressBar.create("Downloading Apps  ", appIds.toSet.size.toLong)
    val failures = MMap[String, String]()
    try {
      val appNames = new File(folder)
        .listFiles()
        .filter(_.isFile)
        .map(_.getName.split("-").head)
        .toSet
      appIds.toSet.foreach { id =>
        try {
          os match {
            case de.tubs.cs.ias.OperatingSystems.ANDROID =>
              if (!appNames.contains(id)) {
                AndroidAppDownloader.download(id, folder, conf.android) match {
                  case x: AndroidAppDownloader.Panic =>
                    error(s"$id -> ${x.getMessage}")
                    failures.addOne(id -> x.getMessage)
                  case AndroidAppDownloader.Result(_) =>
                    throw new RuntimeException(
                      "download does not return a result")
                  case AndroidAppDownloader.Success =>
                  case _: AndroidAppDownloader.InfoResult =>
                    throw new RuntimeException(
                      "this should never be fed back into this function")
                }
              }
            case de.tubs.cs.ias.OperatingSystems.IOS =>
              throw new NotImplementedError("we dropped iOS support for now")
          }
        } catch {
          case e: Exception =>
            error(
              s"$id -> ${e.getMessage} \n ${e.getStackTrace.mkString("\n")}")
            failures.addOne(id -> e.getMessage)
          case e: Error =>
            error(s"$id -> ${e.getMessage}")
            failures.addOne(id -> e.getMessage)
        } finally {
          bar.step()
          if (bar.getCurrent % 100 == 0)
            sendTelegramMessage(
              s"current label download progress: ${bar.getCurrent}")
        }
      }
    } finally {
      bar.close()
    }
    ActionReport(appIds.length - failures.size, failures.size, failures.toMap)
  }

}
