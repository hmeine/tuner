package tuner.project

import tuner.Config
import tuner.Region
import tuner.ViewInfo

trait Viewable extends Project {

  // The visual controls
  val viewInfo = ViewInfo.fromJson(this, config.currentVis)

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

}

