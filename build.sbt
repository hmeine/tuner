
// assembly task
import AssemblyKeys._

assemblySettings

seq(Revolver.settings: _*)

name := "Tuner"

version := "0.2"

scalaVersion := "2.9.1"

resolvers ++= Seq(
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies += "org.bitbucket.gabysbrain" %% "datescala" % "0.9"

libraryDependencies <<= (scalaVersion, libraryDependencies) {(sv, deps) =>
  deps :+ ("org.scala-lang" % "scala-swing" % sv)
}

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.0"

libraryDependencies += "net.liftweb" %% "lift-json" % "2.4"

libraryDependencies += "tablelayout" % "TableLayout" % "20050920"

libraryDependencies += "org.prefuse" % "prefuse" % "beta-20060220"

libraryDependencies += "org.jblas" % "jblas" % "1.2.3"

libraryDependencies += "org.japura" % "japura" % "1.15.1" from "http://downloads.sourceforge.net/project/japura/Japura/Japura%20v1.15.1/japura-1.15.1.jar"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0.M4" % "test"

scalacOptions := Seq("-deprecation", "-unchecked")

javacOptions := Seq("-Xlint:deprecation")

fork := true

javaOptions := {
  val openglPath = "lib/opengl/macosx"
  val jriPath = "/Library/Frameworks/R.framework/Versions/Current/Resources/library/rJava/jri"
  //Seq("-Djava.library.path=" + jriPath + ":" + openglPath)
  Seq("-Djava.library.path=" + jriPath, "-Xmx4G")
  //val openglPath = """lib\opengl\windows64"""
  //val jriPath = """C:\Users\tom\Documents\R\win-library\2.13\rJava\jri"""
  //Seq("-Djava.library.path=" + jriPath + """\x64;""" + jriPath)
}

parallelExecution := false

mainClass := Some("tuner.Tuner")

// testing stalls the assembly build
test in assembly := {}

