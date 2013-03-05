package tuner.project

import tuner.Config
import tuner.HistoryManager
import tuner.Matrix2D
import tuner.Region
import tuner.ViewInfo

trait Viewable extends Project {

  // The visual controls
  val viewInfo = ViewInfo.fromJson(this, config.currentVis)

  def statusString = "Ok"
  
  var _region:Region = Region.fromJson(config.currentRegion, this)

  def region : Region = _region
  def region_=(r:Region) = {
    _region = r
  }

  def value(point:List[(String,Float)]) : Map[String,Float]
  def value(point:List[(String,Float)], response:String) : Float
  def uncertainty(point:List[(String,Float)]) : Map[String,Float]
  def uncertainty(point:List[(String,Float)], response:String) : Float
  def expectedGain(point:List[(String,Float)]) : Map[String,Float]
  def expectedGain(point:List[(String,Float)], response:String) : Float

  def minValue(response:String) : Float
  def maxValue(response:String) : Float

  def minUncertainty(response:String) : Float
  def maxUncertainty(response:String) : Float

  def minExpectedGain(response:String) : Float
  def maxExpectedGain(response:String) : Float

  def viewValueFunction : (List[(String,Float)],String)=>Float = 
    viewInfo.currentMetric match {
      case ViewInfo.ValueMetric => value
      case ViewInfo.ErrorMetric => uncertainty
      case ViewInfo.GainMetric  => expectedGain
    }

  def sampleMatrix(xDim:(String,(Float,Float)),
                   yDim:(String,(Float, Float)),
                   response:String,
                   point:List[(String,Float)]) : Matrix2D = {
    val remainingPt = point.filter {case (fld,_) => 
      fld!=xDim._1 && fld!=yDim._1
    }
    val outData = tuner.Sampler.regularSlice(xDim, yDim, viewInfo.estimateSampleDensity)

    // Populate the slice
    outData.rowIds.zipWithIndex.foreach {tmpx =>
      val (xval,x) = tmpx
      outData.colIds.zipWithIndex.foreach {tmpy =>
        val (yval,y) = tmpy
        val samplePt = (xDim._1,xval)::(yDim._1,yval)::remainingPt
        outData.set(x, y, viewValueFunction(samplePt, response))
      }
    }
    outData
  }

  val history:HistoryManager = config.history match {
    case Some(hc) => HistoryManager.fromJson(hc)
    case None     => new HistoryManager
  }
}

