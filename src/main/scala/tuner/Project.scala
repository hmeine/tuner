package tuner

import net.liftweb.json.JsonParser._
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._

import java.io.File
import java.io.FileWriter
import java.util.Date

import scala.io.Source

// Internal config for matching with the json stuff
case class InputSpecification(name:String, minRange:Float, maxRange:Float)
case class OutputSpecification(name:String, minimize:Boolean)
case class ProjConfig(
  name:String,
  scriptPath:String,
  inputs:List[InputSpecification],
  outputs:List[OutputSpecification],
  ignoreFields:List[String]
)

object Project {
  def recent : List[Project] = {
    Config.recentProjects map {Project.fromFile(_)} toList
  }

  def fromFile(path:String) = {
    new Project(Some(path))
  }

  object Status extends Enumeration {
    val Ok = Value("Ok")
    val BuildingGp = Value("Building GP")
  }
}

class Project(var path:Option[String]) {

  def this() = this(None)

  // Serializers to get the json parser to work
  implicit val formats = net.liftweb.json.DefaultFormats

  var config = path map {p =>
    val projConfigPath = p + "/" + Config.projConfigFilename
    val json = parse(Source.fromFile(projConfigPath).mkString)
    json.extract[ProjConfig]
  }

  var name : String = config match {
    case Some(c) => c.name
    case None    => "New Project"
  }
  var scriptPath : Option[String] = config map {_.scriptPath}
  def modificationDate : Date = new Date
  var status : Project.Status.Value = Project.Status.Ok
  var inputs : DimRanges = config match {
    case Some(c) => 
      new DimRanges(c.inputs map {x => 
        (x.name, (x.minRange, x.maxRange))
      } toMap)
    case None    => new DimRanges(Nil.toMap)
  }

  val samples = path.map({p =>
    val sampleFilename = p + "/" + Config.sampleFilename
    Table.fromCsv(sampleFilename)
  }).getOrElse(new Table)

  var designSites = path.map {p =>
    val designSiteFile = p + "/" + Config.designFilename
    Table.fromCsv(designSiteFile)
  }

  var responses : List[(String,Boolean)] = config match {
    case Some(c) => c.outputs map {rf => (rf.name, rf.minimize)}
    case None => Nil
  }

  var ignoreFields : List[String] = config match {
    case Some(c) => c.ignoreFields
    case None => Nil
  }

  val gpModels = path.map {p =>
    val designSiteFile = p + "/" + Config.designFilename
    val gp = new Rgp(designSiteFile)
    responseFields.map {fld =>
      (fld, gp.buildModel(inputFields, fld, Config.errorField))
    } toMap
  }

  def inputFields : List[String] = inputs.dimNames
  def responseFields : List[String] = responses.map(_._1)

  def newFields : List[String] = {
    val knownFields : Set[String] = 
      (responseFields ++ ignoreFields ++ inputFields).toSet
    designSites.get.fieldNames.filter {fn => !knownFields.contains(fn)}
  }

  def addSamples(n:Int) = {
    // TODO: find a better solution than just ignoring the missing inputs
    Sampler.regularGrid(inputs, n, {v => samples.addRow(v)})
    println(n + " samples generated")
  }

  /**
   * Clears out the sample table then adds the samples
   */
  def newSamples(n:Int) = {
    samples.clear
    addSamples(n)
  }

  def savePath : String = path.get

  def save(savePath:String) = {
    path = Some(savePath)

    val json = (
      ("name" -> name) ~
      ("scriptPath" -> scriptPath) ~
      ("inputs" -> inputs.dimNames.map {dn =>
        ("name" -> dn) ~
        ("minRange" -> inputs.min(dn)) ~
        ("maxRange" -> inputs.max(dn))}
      ) ~
      ("outputs" -> responses.map {rf => 
        ("name" -> rf._1) ~
        ("minimize" -> rf._2)}
      ) ~
      ("ignoreFields" -> ignoreFields)
    )

    // Ensure that the project directory exists
    var pathDir = new File(savePath).mkdir

    val jsonPath = savePath + "/" + Config.projConfigFilename
    val outFile = new FileWriter(jsonPath)
    outFile.write(pretty(render(json)))
    outFile.close

    // Also save the samples
    val sampleName = savePath + "/" + Config.sampleFilename
    samples.toCsv(sampleName)
  }

}

