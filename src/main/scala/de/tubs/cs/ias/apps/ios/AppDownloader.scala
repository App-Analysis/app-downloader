package de.tubs.cs.ias.apps.ios

import de.tubs.cs.ias.apps.DownloadReportReader.downloadReportFormat
import de.tubs.cs.ias.apps.DownloadReport
import de.tubs.cs.ias.util.{LoginConfig, FileSystemInteraction => fsi}
import spray.json.JsonParser
import wvlet.log.LogSupport
import java.io.{ByteArrayOutputStream, PrintWriter}
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.sys.process._

object AppDownloader extends LogSupport {

  private def runShellCmdWithTimeout(
      cmd: String,
      timeoutMs: Long = 60000): (Int, String, String) = {
    val stdoutStream = new ByteArrayOutputStream()
    val stderrStream = new ByteArrayOutputStream()
    val stdoutWriter = new PrintWriter(stdoutStream)
    val stderrWriter = new PrintWriter(stderrStream)
    val process =
      cmd.run(ProcessLogger(stdoutWriter.println, stderrWriter.println))
    val future = Future(blocking(process.exitValue()))
    val exitValue = try {
      Await.result(future, duration.Duration(timeoutMs, duration.MILLISECONDS))
    } catch {
      case _: TimeoutException =>
        process.destroy()
        -42 //we define a timeout as minus 42
    }
    stderrWriter.close()
    stdoutWriter.close()
    (exitValue,
     stdoutStream.toString,
     if (exitValue == -42) "PROCESS TIMEOUT" else stderrStream.toString)
  }

  @tailrec
  def downloadAppUsingIpatoolPy(id: String,
                                folder: String,
                                config: LoginConfig,
                                ipatool: String,
                                tries: Int = 5): Option[String] = {
    val cmd = {
      s"python3 $ipatool --json lookup -i $id -c DE download --purchase -e ${config.email} -p ${config.password}${config.twoFA} -o $folder"
    }
    val (exitValue, stdout, stderr) = runShellCmdWithTimeout(cmd)
    if (exitValue == 0) {
      val report = JsonParser(stdout).convertTo[DownloadReport]
      fsi.moveFile(report.downloadedIPA, s"$folder/$id.ipa")
      fsi.writeFile(stdout, s"$folder/$id.json")
      None
    } else if (stderr.contains("Store authenticate failed!")) {
      throw StoreAuthenticationFailure
    } else if (tries > 0) {
      error(cmd)
      error(stderr)
      // in case of error it is quite likely that we ran into some timeout (waiting for 10sec)
      Thread.sleep(10000)
      downloadAppUsingIpatoolPy(id, folder, config, ipatool, tries - 1)
    } else {
      error(cmd)
      error(stderr)
      Some(stderr)
    }
  }

}
