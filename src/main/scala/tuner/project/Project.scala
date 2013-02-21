package tuner.project

import net.liftweb.json._
import net.liftweb.json.Extraction._

import java.io.File
import java.io.FileWriter
import java.util.Date

import scala.actors.Actor
import scala.actors.Actor._
import scala.collection.immutable.SortedMap
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

import tuner.CandidateGenerator
import tuner.Config
import tuner.ConsoleLine
import tuner.DimRanges
import tuner.FunctionCompiler
import tuner.GpModel
import tuner.GpSpecification
import tuner.HistorySpecification
import tuner.InterpretedFunction
import tuner.PreviewImages
import tuner.Progress
import tuner.ProgressComplete
import tuner.Region
import tuner.RegionSpecification
import tuner.Rgp
import tuner.SampleRunner
import tuner.SamplesCompleted
import tuner.SamplingComplete
import tuner.SamplingError
import tuner.Table
import tuner.ViewInfo
import tuner.VisInfo
import tuner.error.ProjectLoadException
import tuner.util.Density2D
import tuner.util.Path

// Internal config for matching with the json stuff
sealed abstract trait ProjInfo
case class InputSpecification(name:String, minRange:Float, maxRange:Float)
case class OutputSpecification(name:String, minimize:Boolean)
case class SimProjInfo(
  scriptPath:String,
  var gpModels:List[GpSpecification],
  buildInBackground:Boolean
) extends ProjInfo
case class FuncProjInfo(
  functionSrc:String,
  minValue:Float,
  maxValue:Float
) extends ProjInfo
case class ProjConfig(
  name:String,
  inputs:List[InputSpecification],
  var outputs:List[OutputSpecification],
  var ignoreFields:List[String],
  var currentVis:VisInfo,
  currentRegion:RegionSpecification,
  history:Option[HistorySpecification],
  projInfo:ProjInfo
)

object Project {

  // Serializers to get the json parser to work
  implicit val formats = net.liftweb.json.DefaultFormats.withHints(
    ShortTypeHints(List(classOf[SimProjInfo], classOf[FuncProjInfo])))

  def recent : Array[Project] = {
    Config.recentProjects flatMap {rp =>
      try {
        Some(Project.fromFile(rp))
      } catch {
        case e:java.io.FileNotFoundException =>
          None
      }
    } toArray
  }

  def fromFile(path:String) : Project = {
    val configFilePath = Path.join(path, Config.projConfigFilename)
    val json = parse(Source.fromFile(configFilePath).mkString)
    val config = try {
      json.extract[ProjConfig]
    } catch {
      case me:net.liftweb.json.MappingException =>
        throw new ProjectLoadException(me.msg, me)
    }

    val sampleFilePath = Path.join(path, Config.sampleFilename)
    val samples = try {
      Table.fromCsv(sampleFilePath)
    } catch {
      case _:java.io.FileNotFoundException => new Table
    }

    val designSitePath = Path.join(path, Config.designFilename)
    val designSites = try {
      Table.fromCsv(designSitePath)
    } catch {
      case _:java.io.FileNotFoundException => new Table
    }

    config.projInfo match {
      case SimProjInfo(scriptPath, gpModels, buildInBackground) =>
        // Figure out how many rows we've built the models on
        val gpDesignRows = gpModels.map(_.designMatrix.length) match {
          case Nil => 0
          case x   => x.min
        }

        val specifiedFields:List[String] = 
          config.inputs.map(_.name) ++ 
          config.outputs.map(_.name) ++ 
          config.ignoreFields
    
        if(samples.numRows > 0) {
          new RunningSamples(config, path, samples, designSites)
        } else if(gpDesignRows < designSites.numRows) {
          new BuildingGp(config, path, designSites)
        } else if(!designSites.fieldNames.diff(specifiedFields).isEmpty) {
          new NewSimResponses(config, path, designSites.fieldNames)
        } else {
          new SimViewable(config, path, designSites)
        }
      case FuncProjInfo(functionSrc, mnv, mxv) =>
        val func = FunctionCompiler.compile(functionSrc)
        new FunctionProject(config, path, func) {
          def minValue(response:String) : Float = mnv
          def maxValue(response:String) : Float = mxv
        }
    }
  }

  def mapInputs(inputs:List[(String,Float,Float)]) = 
    inputs.map {case (fld, mn, mx) =>
      InputSpecification(fld, mn, mx)
    }
}

trait Project {
  
  val config:ProjConfig

  // Serializers to get the json parser to work
  implicit val formats = net.liftweb.json.DefaultFormats.withHints(
    ShortTypeHints(List(classOf[SimProjInfo], classOf[FuncProjInfo]))
  )
  
  def save(savePath:String) : Unit = {
    // Ensure that the project directory exists
    var pathDir = new File(savePath).mkdir

    val jsonPath = Path.join(savePath, Config.projConfigFilename)
    val outFile = new FileWriter(jsonPath)
    outFile.write(pretty(render(decompose(config))))
    outFile.close
  }

  /**
   * The next stage in the project's evolution
   */
  def next : Project

  val name = config.name

  val inputs = new DimRanges(config.inputs.map {x =>
    (x.name -> (x.minRange, x.maxRange))
  } toMap)

  val modificationDate:Date

  def statusString:String

  def responses = config.outputs.map {x => (x.name, x.minimize)}

  def ignoreFields = config.ignoreFields.sorted

  def inputFields = inputs.dimNames.sorted
  def responseFields = responses.map(_._1).sorted

}

trait SimProject extends Project {

  override def save(savePath:String) : Unit = {
    super.save(savePath)

    // Try to save the sample tables
    this match {
      case s:Sampler => s.saveSampleTables(savePath)
      case _         => 
    }
  }

  val scriptPath = config.projInfo.asInstanceOf[SimProjInfo].scriptPath
}

trait InProgress extends SimProject with Actor {

  var buildInBackground:Boolean

  //def start:Unit
  def stop:Unit

  private var eventListeners:ArrayBuffer[Actor] = ArrayBuffer()
  def addListener(a:Actor) = eventListeners.append(a)
  protected def publish(o:Any) = eventListeners.foreach {a => a ! o}
}

class NewSimProject(name:String, 
                    basePath:String,
                    scriptPath:String, 
                    inputDims:List[(String,Float,Float)]) 
               extends SimProject with Sampler {
                 
                 
  val config = ProjConfig(name, 
                          Project.mapInputs(inputDims),
                          Nil, Nil, 
                          ViewInfo.Default,
                          Region.Default,
                          None,
                          SimProjInfo(scriptPath, Nil, false))

  val path = Path.join(basePath, name)

  val modificationDate = new Date

  val newSamples = new Table
  val designSites = new Table

  override def save(savePath:String) = {
    super.save(savePath)

    val sampleName = Path.join(savePath, Config.sampleFilename)
    newSamples.toCsv(sampleName)
  }

  def statusString = "New"

  def sampleRanges = 
    new DimRanges(inputDims.map(x => (x._1, (x._2, x._3))).toMap)
  
  def next = {
    save(path)
    //Project.fromFile(path).asInstanceOf[RunningSamples]
    Project.fromFile(path)
  }
}

class RunningSamples(val config:ProjConfig, val path:String, 
                     val newSamples:Table, val designSites:Table) 
    extends InProgress with Saved {
  
  var buildInBackground:Boolean = 
    config.projInfo.asInstanceOf[SimProjInfo].buildInBackground

  // See if we should start running some samples
  var sampleRunner:Option[SampleRunner] = None 
  if(buildInBackground) start

  def statusString = 
    "Running Samples (%s/%s)".format(currentTime.toString, totalTime.toString)
  
  val totalTime = newSamples.numRows
  var currentTime = 0

  override def save(savePath:String) = {
    super.save(savePath)

    // Also save the samples
    val sampleName = Path.join(savePath, Config.sampleFilename)
    newSamples.toCsv(sampleName)

    // Also save the design points
    val designName = Path.join(savePath, Config.designFilename)
    designSites.toCsv(designName)
  }

  override def start = {
    runSamples
    super.start
  }
  def stop = {}

  def next = {
    save()
    Project.fromFile(path).asInstanceOf[BuildingGp]
  }

  def act = {
    // Publish one of these right at the beginning just to get things going
    publish(Progress(currentTime, totalTime, statusString, true))

    var finished = false
    loopWhile(!finished) {
      react {
        case x:ConsoleLine => 
          publish(x)
        case SamplesCompleted(num) => 
          currentTime += num
          publish(Progress(currentTime, totalTime, statusString, true))
        case SamplingError(exitCode) =>
          val msg = "Sampler script exited with code: " + exitCode
          publish(Progress(currentTime, totalTime, msg, false))
        case SamplingComplete =>
          finished = true
          publish(ProgressComplete)
      }
    }
  }

  private def runSamples = {
    // only run if we aren't running something
    if(!sampleRunner.isDefined && newSamples.numRows > 0) {
      val runner = new SampleRunner(this)
      runner.start
      sampleRunner = Some(runner)
    }
  }
}

class BuildingGp(val config:ProjConfig, val path:String, designSites:Table) 
    extends InProgress with Saved {
  
  var buildInBackground:Boolean = 
    config.projInfo.asInstanceOf[SimProjInfo].buildInBackground
  
  //val gps = responseFields.map(fld => (fld, loadGpModel(gp, fld))).toMap

  def statusString = "Building GP"

  def stop = {}

  def act = {
    publish(Progress(-1, -1, statusString, true))
    // Build the gp models
    val designSiteFile = Path.join(path, Config.designFilename)
    val gp = new Rgp(designSiteFile)

    val buildFields = designSites.fieldNames.diff(inputFields++ignoreFields)

    val newModels = buildFields.map({fld => 
      println("building model for " + fld)
      (fld, gp.buildModel(inputFields, fld, Config.errorField))
    }).toMap
    config.projInfo.asInstanceOf[SimProjInfo].gpModels = 
      newModels.values.map(_.toJson).toList
    save()
    publish(ProgressComplete)
  }

  def next = {
    save()
    Project.fromFile(path)
  }

  override def save(savePath:String) = {
    super.save(savePath)

    // create an empty model samples table
    val fakeTable = new Table
    val filepath = Path.join(savePath, Config.respSampleFilename)
    fakeTable.toCsv(filepath)
  }

}

class NewSimResponses(val config:ProjConfig, val path:String, allFields:List[String])
    extends SimProject with Saved {
  
  def statusString = "New Responses"

  def addResponse(field:String, minimize:Boolean) = {
    if(!responseFields.contains(field)) {
      config.outputs = OutputSpecification(field, minimize) :: config.outputs
    }
  }

  def addIgnore(field:String) = {
    if(!ignoreFields.contains(field)) {
      config.ignoreFields = field :: config.ignoreFields
    }
  }

  def newFields : List[String] = {
    val knownFields : Set[String] = 
      (responseFields ++ ignoreFields ++ inputFields).toSet
    allFields.filter {fn => !knownFields.contains(fn)}
  }

  def next = {
    save
    Project.fromFile(path).asInstanceOf[SimViewable]
  }

}

class SimViewable(val config:ProjConfig, val path:String, val designSites:Table) 
    extends SimProject with Viewable with Saved with Sampler {

  import Project._

  val newSamples = new Table

  val gpModels:SortedMap[String,GpModel] = SortedMap[String,GpModel]() ++
    config.projInfo.asInstanceOf[SimProjInfo].gpModels.map {gpConfig =>
      (gpConfig.responseDim, GpModel.fromJson(gpConfig))
    }

  val candidateGenerator = new CandidateGenerator(this)

  val previewImages:Option[PreviewImages] = loadImages(path)

  // Also set up a table of samples from each gp model
  val modelSamples:Table = loadResponseSamples(path)

  save()

  override def save(savePath:String) : Unit = {
    // Update the view info
    config.currentVis = viewInfo.toJson

    super.save(savePath)

    // Save the model samples
    if(modelSamples.numRows > 0) {
      val filepath = Path.join(savePath, Config.respSampleFilename)
      modelSamples.toCsv(filepath)
    }
  }

  def next = {
    save()
    Project.fromFile(path).asInstanceOf[RunningSamples]
  }

  def sampleRanges = _region.toRange

  def numSamplesInRegion = {
    var count = 0
    for(i <- 0 until designSites.numRows) {
      val tpl = designSites.tuple(i)
      val inputs = tpl.filterKeys {k => inputFields contains k}
      if(region.inside(inputs.toList))
        count += 1
    }
    count
  }

  /**
   * The number of sample points that are unclipped 
   * by the current zoom level
   */
  def numUnclippedPoints : Int = {
    val (active,_) = viewFilterDesignSites
    active.numRows
  }

  def newFields : List[String] = {
    val knownFields : Set[String] = 
      (responseFields ++ ignoreFields ++ inputFields).toSet
    designSites.fieldNames.filter {fn => !knownFields.contains(fn)}
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

    var (minDist, minRow) = (Double.MaxValue, designSites.tuple(0))
    for(r <- 0 until designSites.numRows) {
      val tpl = designSites.tuple(r)
      val dist = ptDist(tpl)
      if(dist < minDist) {
        minDist = dist
        minRow = tpl
      }
    }
    minRow.toList
  }

  def viewFilterDesignSites : (Table,Table) = {
    val active = new Table
    val inactive = new Table
    for(r <- 0 until designSites.numRows) {
      val tpl = designSites.tuple(r).toList
      if(viewInfo.inView(tpl)) active.addRow(tpl)
      else                     inactive.addRow(tpl)
    }
    (active, inactive)
  }

  def estimatePoint(point:List[(String,Float)]) 
        : Map[String,(Float,Float,Float)] = {
    gpModels.map {case (fld, model) =>
      val (est, err) = model.runSample(point)
      (fld -> (est.toFloat, err.toFloat, 
               model.calcExpectedGain(est, err).toFloat))
    }
  }

  def value(point:List[(String,Float)]) : Map[String,Float] = {
    estimatePoint(point) map {case (k,v) => (k -> v._1)}
  }

  def value(point:List[(String,Float)], response:String) : Float = {
    gpModels(response).runSample(point)._1.toFloat
  }

  /**
   * Use the std deviation for the model as the "official" uncertainty
   */
  def uncertainty(point:List[(String,Float)]) : Map[String,Float] = {
    estimatePoint(point) map {case (k,v) => (k -> v._2)}
  }

  /**
   * Use the std deviation for the model as the "official" uncertainty
   */
  def uncertainty(point:List[(String,Float)], response:String) : Float = {
    gpModels(response).runSample(point)._2.toFloat
  }

  def expectedGain(point:List[(String,Float)]) : Map[String,Float] = 
    estimatePoint(point) map {case (k,v) => 
      (k -> gpModels(k).calcExpectedGain(v._1, v._2).toFloat)
    }

  def expectedGain(point:List[(String,Float)], response:String) : Float = {
    val sample = gpModels(response).runSample(point)
    gpModels(response).calcExpectedGain(sample._1, sample._2).toFloat
  }

  def minValue(response:String) = gpModels(response).funcMin
  def maxValue(response:String) = gpModels(response).funcMax

  def minUncertainty(response:String) = 0f
  def maxUncertainty(response:String) = {
    math.sqrt(gpModels(response).sig2).toFloat
  }

  def minExpectedGain(response:String) = 0f
  def maxExpectedGain(response:String) = {
    gpModels(response).maxGain(inputs) / 4
  }

  def randomSample2dResponse(resp1Dim:(String,(Float,Float)), 
                             resp2Dim:(String,(Float,Float))) = {

    val numSamples = viewInfo.estimateSampleDensity * 2
    Density2D.density(modelSamples, numSamples, resp2Dim, resp1Dim)
  }

  private def loadResponseSamples(path:String) : Table = {
    // First try to load up an old file
    val samples = try {
      val filepath = Path.join(path, Config.respSampleFilename)
      val tmp = Table.fromCsv(filepath)
      if(tmp.numRows == 0) {
        tuner.Sampler.lhc(inputs, Config.numericSampleDensity)
      } else {
        tmp
      }
    } catch {
      case e:java.io.FileNotFoundException => 
        tuner.Sampler.lhc(inputs, Config.numericSampleDensity)
    }
    gpModels.foldLeft(samples) {case (tbl, (fld, model)) =>
      if(!tbl.fieldNames.contains(fld)) {
        println("sampling response " + fld)
        gpModels(fld).sampleTable(tbl)
      } else {
        tbl
      }
    }
  }

  private def loadImages(path:String) : Option[PreviewImages] = {
    if(!gpModels.isEmpty) {
      val model = gpModels.values.head
      val imagePath = Path.join(path, Config.imageDirname)
      try {
        Some(new PreviewImages(model, imagePath, designSites))
      } catch {
        case e:java.io.FileNotFoundException => 
          //e.printStackTrace
          println("Could not find images, disabling")
          None
      }
    } else {
      None
    }
  }

}

abstract class FunctionProject(val config:ProjConfig, val path:String, 
                      function:InterpretedFunction)
    extends Project with Viewable with Saved {

  def value(point:List[(String,Float)]) : Map[String,Float] = 
    Map("y" -> value(point, "y"))

  def value(point:List[(String,Float)], response:String) : Float = 
    function(orderedPoint(point))

  def uncertainty(point:List[(String,Float)]) : Map[String,Float] = 
    Map("y" -> uncertainty(point, "y"))

  def uncertainty(point:List[(String,Float)], response:String) : Float = 0f

  def expectedGain(point:List[(String,Float)]) : Map[String,Float] = 
    Map("y" -> uncertainty(point, "y"))

  def expectedGain(point:List[(String,Float)], response:String) : Float = 0f

  def minUncertainty(response:String) : Float = 0f
  def maxUncertainty(response:String) : Float = 0f

  def minExpectedGain(response:String) : Float = 0f
  def maxExpectedGain(response:String) : Float = 0f

  // These will come in handy later for variance estimates and such
  val functionSamples = {
    val samples = tuner.Sampler.lhc(inputs, Config.numericSampleDensity)
    val tbl = new Table
    for(r <- 0 until samples.numRows) {
      val tpl = samples.tuple(r).toList
      val y = value(tpl, "y")
      tbl.addRow(("y", y)::tpl)
    }
    tbl
  }

  // overall mean of the function
  val mean = {
    var ttl = 0f
    for(r <- 0 until functionSamples.numRows) {
      ttl += functionSamples.tuple(r)("y")
    }
    ttl / functionSamples.numRows.toFloat
  }

  // overall variance of the function
  val variance:Float = {
    var ttl = 0f
    for(r <- 0 until functionSamples.numRows) {
      ttl += math.pow(functionSamples.tuple(r)("y") - mean, 2).toFloat
    }
    ttl / functionSamples.numRows.toFloat
  }

  override def save(savePath:String) : Unit = {
    // Update the view info
    config.currentVis = viewInfo.toJson

    super.save(savePath)
  }

  // Next does nothing ... NOTHING!
  def next = this

  def localSensitivities : List[(String, Map[String,Float])] = List(
    ("gradient", inputFields map {fld => 
      (fld -> centralDiffGradient(viewInfo.currentSlice.toList, fld))
    } toMap),
    ("value-normed gradient", inputFields map {fld => 
      (fld -> valueNormedGradient(viewInfo.currentSlice.toList, fld))
    } toMap),
    ("mean-normed gradient", inputFields map {fld => 
      (fld -> meanNormedGradient(viewInfo.currentSlice.toList, fld))
    } toMap),
    ("variance-normed gradient", inputFields map {fld => 
      (fld -> varianceNormedGradient(viewInfo.currentSlice.toList, fld))
    } toMap),
    ("factor fixing variance", inputFields map {fld => 
      (fld -> factorFixingVariance(viewInfo.currentSlice.toList, fld))
    } toMap)
  )

  def centralDiffGradient(point:List[(String,Float)], fld:String) : Float = {
    val (x, remPt) = point partition {_._1 == fld}
    val (_, ctr) = x(0)
    val mnv = value((fld,ctr-Config.epsilon)::remPt, "y")
    val mxv = value((fld,ctr+Config.epsilon)::remPt, "y")

    (mxv-mnv) / (2f*Config.epsilon)
  }

  def valueNormedGradient(point:List[(String,Float)], fld:String) : Float = {
    val x = point.find(_._1 == fld).get._2
    val y = value(point, "y")
    (x/y) * centralDiffGradient(point, fld)
  }

  def meanNormedGradient(point:List[(String,Float)], fld:String) : Float = {
    val x = inputs.mean(fld)
    val y = mean
    (x/y) * centralDiffGradient(point, fld)
  }


  def varianceNormedGradient(point:List[(String,Float)], fld:String) : Float = {
    val x = inputs.variance(fld)
    val y = variance
    (x/y) * centralDiffGradient(point, fld)
  }

  def factorFixingVariance(point:List[(String,Float)], fld:String) : Float = {
    val x = point.find(_._1 == fld).get._2
    val meanValue = value(
      inputFields map {f => if(f==fld) (f, x) else (f, inputs.mean(f))},
      "y"
    )
    var ttl = 0f
    for(r <- 0 until functionSamples.numRows) {
      val tpl = functionSamples.tuple(r)
      val remPt = tpl.filterKeys {k => k!=fld} toList

      ttl += math.pow(value((fld,x)::remPt, "y") - meanValue, 2).toFloat
    }
    ttl / functionSamples.numRows.toFloat
  }

  private def orderedPoint(point:List[(String,Float)]) : List[Float] = {
    val pt = point.toMap
    inputFields map {fld => pt(fld)}
  }
}

