package de.tubs.cs.ias.download

import spray.json.{DefaultJsonProtocol, JsonParser, RootJsonFormat}

import scala.io.Source

case class DownloaderConfig(remoteHost: String,
                            remoteUser: String,
                            remoteAndroid: String,
                            remoteIos: String) {

  def sshString: String = s"$remoteUser@$remoteHost"

}

object DownloaderConfig extends DefaultJsonProtocol {

  implicit val downloaderConfigFormat: RootJsonFormat[DownloaderConfig] =
    jsonFormat4(DownloaderConfig.apply)

  def apply(path: String): DownloaderConfig = {
    val source = Source.fromFile(path)
    try {
      JsonParser(source.mkString).convertTo[DownloaderConfig]
    } finally {
      source.close()
    }
  }

}
