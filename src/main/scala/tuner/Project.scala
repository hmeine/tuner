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
case class GpSpecification(
  responseDim:String,
  dimNames:List[String],
  thetas:List[Double],
  alphas:List[Double],
  mean:Double,
  sigma2:Double,
  designMatrix:List[List[Double]],
  responses:List[Double],
  invCorMtx:List[List[Double]]
)
case class ProjConfig(
  name:String,
  scriptPath:String,
  inputs:List[InputSpecification],
  outputs:List[OutputSpecification],
  ignoreFields:List[String],
  gpModels:List[GpSpecification],
  currentVis:VisInfo,
  currentRegion:RegionSpecification,
  history:Option[HistorySpecification]
)

object Project {
  def recent : List[Project] = {
    Config.recentProjects map {Project.fromFile(_)} toList
  }

  def fromFile(path:String) = {
    new Project(Some(path))
  }

  sealed trait Status {
    def statusString
  }
  case object Ok extends Status {
    def statusString = "Ok"
  }
  case object NeedsInitialSamples extends Status {
    def statusString = "Needs Initial Samples"
  }
  case class RunningSamples(numDone:Int, total:Int) extends Status {
    def statusString = numDone + "/" + total + " Samples Done"
  }
  case object BuildingGp extends Status {
    def statusString = "Building GP"
  }

  sealed trait MetricView
  case object ValueMetric extends MetricView
  case object ErrorMetric extends MetricView
  case object GainMetric extends MetricView
}

class Project(var path:Option[String]) {

  import Project._

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

  // The visual controls
  val viewInfo = config match { 
    case Some(c) => ViewInfo.fromJson(this, c.currentVis)
    case None    => new ViewInfo(this)
  }

  val gpModels : Option[Map[String,GpModel]] = path.map {p =>
    val designSiteFile = p + "/" + Config.designFilename
    val gp = new Rgp(designSiteFile)
    responseFields.map {fld => (fld, loadGpModel(gp, fld))} toMap
  }

  var _region:Region = config match {
    case Some(c) => Region.fromJson(c.currentRegion, this)
    case None    => Region(Region.Box, this)
  }

  val history:HistoryManager = config match {
    case Some(c) => c.history match {
      case Some(hc) => HistoryManager.fromJson(hc)
      case None     => new HistoryManager
    }
    case None => new HistoryManager
  }

  val candidateGenerator = new CandidateGenerator(this)

  // Save any gp models that got updated
  path.foreach(_ => save(savePath))

  def status : Project.Status = {
    if(samples.numRows == 0) {
      Project.NeedsInitialSamples
    } else if(!gpModels.isDefined) {
      Project.BuildingGp
    } else {
      Project.Ok
    }
  }

  def inputFields : List[String] = inputs.dimNames.sorted
  def responseFields : List[String] = responses.map(_._1).sorted

  def region : Region = _region
  def region_=(r:Region) = {
    _region = r
  }

  def newFields : List[String] = {
    val knownFields : Set[String] = 
      (responseFields ++ ignoreFields ++ inputFields).toSet
    designSites.get.fieldNames.filter {fn => !knownFields.contains(fn)}
  }

  def addSamples(n:Int, method:Sampler.Method) = {
    // TODO: find a better solution than just ignoring the missing inputs
    if(n > 0) {
      method(inputs, n, {v => samples.addRow(v)})
      println(n + " samples generated")
    }
  }

  /**
   * Clears out the sample table then adds the samples
   */
  def newSamples(n:Int, method:Sampler.Method) = {
    samples.clear
    addSamples(n, method)
  }

  def updateCandidates(newValues:List[(String,Float)]) = {
    candidateGenerator.update(newValues)
  }

  def candidates = candidateGenerator.candidates

  def candidateFilter = candidateGenerator.currentFilter

  def closestSample(point:List[(String,Float)]) : List[(String,Float)] = {
    def ptDist(tpl:Table.Tuple) : Double = {
      val diffs = point.map {case (fld, v) =>
        math.pow(tpl.getOrElse(fld, Float.MaxValue) - v, 2)
      }
      math.sqrt(diffs.sum)
    }
    designSites match {
      case Some(t) => t.data.foldLeft(Double.MaxValue, t.tuple(0))((mi,r) => {
        val dist = ptDist(r)
        if(dist < mi._1) (dist, r)
        else             mi
      })._2.toList
      case None => Nil
    }
  }

  def estimatePoint(point:List[(String,Float)]) 
        : Map[String,(Float,Float,Float)] = {
    gpModels match {
      case Some(models) => models.map {case (fld, model) =>
        val (est, err) = model.runSample(point)
        (fld -> (est.toFloat, err.toFloat, 
                 model.calcExpectedGain(est.toFloat, err.toFloat)))
      }
      case None         => Map()
    }
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
      ("ignoreFields" -> ignoreFields) ~
      ("gpModels" -> (gpModels match {
        case Some(models) => models.map {case (fld, model) =>
          ("responseDim" -> fld) ~
          ("dimNames" -> model.dims) ~
          ("thetas" -> model.thetas) ~
          ("alphas" -> model.alphas) ~
          ("mean" -> model.mean) ~
          ("sigma2" -> model.sig2) ~
          ("designMatrix" -> model.design.map(_.toList).toList) ~
          ("responses" -> model.responses.toList) ~
          ("invCorMtx" -> model.rInverse.map(_.toList).toList)
        } toList
        case None         => Nil
      })) ~
      ("currentVis" -> viewInfo.toJson) ~
      ("currentRegion" -> region.toJson) ~
      ("history" -> history.toJson)
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

  private def loadGpModel(factory:Rgp, field:String) : GpModel = {
    val gpConfig:Option[GpSpecification] = config match {
      case Some(c) => c.gpModels.find(_.responseDim==field)
      case None    => None
    }
    gpConfig match {
      case Some(c) => 
        new GpModel(c.thetas, c.alphas, c.mean, c.sigma2,
                    c.designMatrix.map {_.toArray} toArray, 
                    c.responses.toArray,
                    c.invCorMtx.map {_.toArray} toArray, 
                    c.dimNames, c.responseDim, Config.errorField)
      case None    => 
        println("building model for " + field)
        factory.buildModel(inputFields, field, Config.errorField)
    }
  }

}

