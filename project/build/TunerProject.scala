import sbt._

class TunerProject(info : ProjectInfo) extends DefaultProject(info) {

  val scalaToolsSnapshots = ScalaToolsSnapshots

  val scalaSwing = "org.scala-lang" % "scala-swing" % "2.8.1"
  val lift_json = "net.liftweb" % "lift-json_2.8.0" % "2.1"
  val tablelayout = "tablelayout" % "TableLayout" % "20050920"
  val commonsMath = "org.apache.commons" % "commons-math" % "2.0"

  val jriPath = "/Library/Frameworks/R.framework/Versions/Current/Resources/library/rJava/jri"

  override def compileOptions : Seq[CompileOption] = List(Deprecation)

  override def unmanagedClasspath : PathFinder = {
    super.unmanagedClasspath +++ 
      Path.fromFile(jriPath + "/JRI.jar") +++
      Path.fromFile(jriPath + "/JRIEngine.jar") +++
      Path.fromFile(jriPath + "/REngine.jar")
  }

  override def mainClass : Option[String] = Some("tuner.Tuner")
  
}

