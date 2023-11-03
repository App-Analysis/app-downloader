package de.tubs.cs.ias.util

import de.tubs.cs.ias.util.{FileSystemInteraction => fsi}
import spray.json._

object Config extends DefaultJsonProtocol {

  implicit val appCategoryFormat: RootJsonFormat[AppCategory] = jsonFormat2(
    AppCategory)

  implicit val loginConfigFormat: RootJsonFormat[LoginConfig] = jsonFormat3(
    LoginConfig)

  implicit val telegramConfigFormat: RootJsonFormat[TelegramConfig] =
    jsonFormat3(TelegramConfig)

  implicit val iosConfigFormat: RootJsonFormat[iOSConfig] = jsonFormat4(
    iOSConfig)

  implicit val androidConfigFormat: RootJsonFormat[AndroidConfig] =
    jsonFormat3(AndroidConfig)

  implicit val configFormat: RootJsonFormat[Config] = jsonFormat6(Config.apply)

  def read(path: String): Config = {
    fsi.readInTextFile(path).parseJson.convertTo[Config]
  }

}

case class AppCategory(name: String, id: String)

case class LoginConfig(email: String, password: String, twoFA: String)

case class TelegramConfig(enable: Boolean, apiKey: String, chatId: String)

case class iOSConfig(categories: List[AppCategory],
                     login: LoginConfig,
                     ipatoolpy: String,
                     osFolderName: String)

case class AndroidConfig(categories: List[AppCategory],
                         osFolderName: String,
                         googleplay: String)

case class Config(ios: iOSConfig,
                  android: AndroidConfig,
                  telegram: TelegramConfig,
                  maxAmountApps: Int,
                  downloadFolderRoot: String,
                  downloaderConfig: String)
