import sbt.Resolver
import com.lihaoyi.workbench.WorkbenchPlugin.workbenchSettings

enablePlugins(ScalaJSPlugin)
enablePlugins(WorkbenchPlugin)

workbenchSettings

scalaVersion := "2.12.4"

name    := "weather2"
version := "1.0"

scalacOptions ++= Seq("-deprecation", "-feature")

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom"       % "0.9.4"
 ,"org.scala-js" %%% "scalajs-java-time" % "0.2.3"
 ,"org.akka-js"  %%% "akkajsactor"       % "1.2.5.0"
 ,"org.querki"   %%% "querki-jsext"      % "0.8"
)

// Redirect ScalaJS compiled output to the classes directory
crossTarget in fastOptJS := baseDirectory.value / "target/scala-2.12/classes"
crossTarget in fullOptJS := baseDirectory.value / "target/scala-2.12/classes"
