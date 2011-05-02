import sbt._

class TunerProject(info : ProjectInfo) extends DefaultProject(info) {

  val scalaToolsSnapshots = ScalaToolsSnapshots
  val scalaSwing = "org.scala-lang" % "scala-swing" % "2.8.1"

  val lift_json = "net.liftweb" % "lift-json" % "2.0"

  //val tablelayout = "tablelayout" % "TableLayout" % "20050920"

  override def compileOptions : Seq[CompileOption] = List(Deprecation)

  override def mainClass : Option[String] = Some("tuner.Tuner")
  
}
