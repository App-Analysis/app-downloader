package de.tubs.cs.ias.applist

import de.tubs.cs.ias.OperatingSystems.OperatingSystem
import de.tubs.cs.ias.applist.ios.AppListDownloader.{
  download => iosAppListDownload
}
import de.tubs.cs.ias.applist.android.AppListDownloader.{
  download => androidAppListDownload
}
import AppListParser.appListFormat
import de.tubs.cs.ias.util.{
  ActionReport,
  AppCategory,
  AsciiProgressBar,
  FileSystemInteraction => fsi
}
import spray.json.JsonParser
import wvlet.log.LogSupport

import java.io.File

object AppListAction extends LogSupport {

  def readInAppList(path: String): MobileAppList = {
    JsonParser(fsi.readInTextFile(path)).convertTo[MobileAppList]
  }

  /** download the chart lists for ios
    *
    * @param operatingSystem the operating system for which to download the charts
    * @param categories the categories for which to download the lists
    * @param folder the folder where to store the lists
    * @return the map of downloaded lists as well as an action report
    */
  def download(operatingSystem: OperatingSystem,
               categories: List[AppCategory],
               folder: String): (Map[String, MobileAppList], ActionReport) = {
    assert(folder.charAt(folder.length - 1) == '/')
    val bar =
      AsciiProgressBar.create("Downloading Charts", categories.length.toLong)
    val ret = try {
      categories.map {
        case AppCategory(name, id) =>
          operatingSystem match {
            case de.tubs.cs.ias.OperatingSystems.IOS =>
              val file = s"$folder/$name.json"
              if (!new File(file).exists())
                fsi.writeFile(iosAppListDownload(id), file)
              bar.step()
              name -> readInAppList(file)
            case de.tubs.cs.ias.OperatingSystems.ANDROID =>
              val file = s"$folder/$name.json"
              if (!new File(file).exists())
                fsi.writeFile(androidAppListDownload(id), file)
              bar.step()
              name -> readInAppList(file)
          }
      }.toMap
    } finally {
      bar.close()
    }
    (ret, ActionReport(categories.length, 0, Map()))
  }

}
