ThisBuild / scalaVersion := "3.6.2"
organization := "de.tubs.cs.ias"

enablePlugins(JavaAppPackaging)

libraryDependencies ++= Seq(
  "org.scalatest"                 %% "scalatest"               % "3.2.19" % Test,
  "de.halcony"                    %% "scala-argparse"          % "2.0.5",
  "io.spray"                      %% "spray-json"              % "1.3.6",
  "com.typesafe"                  %  "config"                  % "1.4.3",
  "org.wvlet.airframe"            %% "airframe-log"            % "2025.1.6",
  "org.slf4j"                     %  "slf4j-nop"               % "2.0.16",
  "com.softwaremill.sttp.client4" %% "core"                    % "4.0.0-RC1",
  "me.tongfei"                    % "progressbar"              % "0.10.1",
  "org.dispatchhttp"              %% "dispatch-core"           % "2.0.0"
)

ThisBuild / resolvers ++= Seq(
  Resolver.mavenLocal,
  Resolver.mavenCentral,
  "Sonatype OSS Snapshots" at "https://s01.oss.sonatype.org/content/repositories/public",
  "Apache public" at "https://repository.apache.org/content/groups/public/"
)

scalacOptions ++= Seq(
  "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
  "-encoding", "utf-8",                // Specify character encoding used by source files.
  "-explaintypes",                     // Explain type errors in more detail.
  "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
  "-language:higherKinds",             // Allow higher-kinded types
  "-language:implicitConversions",     // Allow definition of implicit functions called views
  "-unchecked",                        // Enable additional warnings where generated code depends on assumptions
)

compile / javacOptions ++= Seq("-Xlint:all", "-Xlint:-cast", "-g")
Test / fork :=  false
Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat
testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-v")

checkstyleConfigLocation := CheckstyleConfigLocation.File("config/checkstyle/google_checks.xml")
checkstyleSeverityLevel := Some(CheckstyleSeverityLevel.Info)