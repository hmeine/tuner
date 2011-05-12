package tuner.gui

import tuner.Config
import tuner.DimRanges
import tuner.GpModel
import tuner.Matrix2D
import tuner.Project
import tuner.SpecifiedColorMap

class MainPlotPanel(project:Project, resp1:Option[String], resp2:Option[String]) 
    extends P5Panel(Config.mainPlotDims._1, Config.mainPlotDims._2, P5Panel.OpenGL) {

  type PlotInfoMap = Map[(String,String), ContinuousPlot]
  type AxisMap = Map[String,Axis]
  // This is the response field, gp model, x axes, y axes, and plots
  type ResponseInfo = (String,GpModel,AxisMap,AxisMap,PlotInfoMap)

  var zoomDims = new DimRanges(project.inputs.ranges)
  var currentSlice:Map[String,Float] = project.inputFields.map {fld =>
    val rng = zoomDims.range(fld)
    (fld, (rng._1 + rng._2) / 2f)
  } toMap

  val resp1Info:Option[ResponseInfo] = resp1 match {
    case Some(r1) => project.gpModels match {
      case Some(gpm) => 
        val model = gpm(r1)
        val cm = new SpecifiedColorMap(Config.response1ColorMap, 
                                       model.funcMin, 
                                       model.funcMax)
        Some((r1, model, createAxes(Axis.HorizontalBottom),
                         createAxes(Axis.VerticalLeft),
                         createPlots(cm)))
      case None      => None
    }
    case None     => None
  }

  val resp2Info:Option[ResponseInfo] = resp2 match {
    case Some(r2) => project.gpModels match {
      case Some(gpm) => 
        val model = gpm(r2)
        val cm = new SpecifiedColorMap(Config.response2ColorMap, 
                                       model.funcMin, 
                                       model.funcMax)
        Some((r2, model, createAxes(Axis.HorizontalTop),
                         createAxes(Axis.VerticalRight),
                         createPlots(cm)))
      case None      => None
    }
    case None     => None
  }

  def sortedDims : List[String] = zoomDims.dimNames.sorted

  def plotData(model:GpModel,
               d1:(String,(Float,Float)), 
               d2:(String,(Float,Float)), 
               slice:Map[String,Float]) : Matrix2D = {
    model.sampleSlice(d1, d2, slice.toList)._1._2
  }

  def draw = {
    // Compute the spacing of everything
    val startTime = System.currentTimeMillis
    val responseSize = height - 
                       ((zoomDims.length-1) * Config.plotSpacing) -
                       (Config.axisSize * 2) -
                       (Config.plotSpacing * 2)
    val sliceSize = responseSize / zoomDims.length - Config.plotSpacing
    val slicesStartX = Config.plotSpacing + Config.axisSize
    val slicesStartY = Config.plotSpacing + Config.axisSize
    // Bottom, top
    val xAxesStart = (slicesStartY + responseSize, Config.plotSpacing)
    // Left, right
    val yAxesStart = (Config.plotSpacing, slicesStartX + responseSize)

    def drawResp1(xf:String, yf:String, x:Float, y:Float) = {
      drawResponse(resp1Info, xf, yf, x, y, 
                   sliceSize, xAxesStart._1, yAxesStart._1)
    }
    def drawResp2(xf:String, yf:String, x:Float, y:Float) = {
      drawResponse(resp2Info, xf, yf, x, y, 
                   sliceSize, xAxesStart._2, yAxesStart._2)
    }

    // Draw the splom itself
    sortedDims.foldLeft(slicesStartX) {case (xPos, xFld) =>
      sortedDims.foldLeft(slicesStartY) {case (yPos, yFld) =>
        if(xFld < yFld) {
          // response1 goes in the lower left
          drawResp1(xFld, yFld, xPos, yPos)
        } else if(xFld > yFld) {
          // response2 goes in the upper right
          // x and y field names here are actually reversed
          drawResp2(yFld, xFld, xPos, yPos)
        }
        yPos + sliceSize + Config.plotSpacing
      }
      xPos + sliceSize + Config.plotSpacing
    }

    val endTime = System.currentTimeMillis
    //println("draw time: " + (endTime - startTime) + "ms")
  }

  private def drawResponse(responseInfo:Option[ResponseInfo], 
                           xFld:String, yFld:String,
                           xPos:Float, yPos:Float, sliceSize:Float,
                           xAxisStart:Float, yAxisStart:Float) = {
    val xRange = (xFld, zoomDims.range(xFld))
    val yRange = (yFld, zoomDims.range(yFld))
    responseInfo foreach {case (field, model, xAxes, yAxes, plots) =>
      val data = plotData(model, xRange, yRange, currentSlice)
      val plot = plots((xFld, yFld))
      //plot.draw(this, xPos, yPos, sliceSize, sliceSize, data)
      // See if we should draw the axes
      if(yFld == sortedDims.last) {
        //println(xFld + ": " + xPos + " " + xAxisStart)
        xAxes(xFld).draw(this, xPos, xAxisStart,
                         sliceSize, Config.axisSize, 
                         xRange)
      }
      if(xFld == sortedDims.head) {
        yAxes(yFld).draw(this, yAxisStart, yPos,
                         Config.axisSize, sliceSize, 
                         yRange)
      }
    }
  }

  private def createPlots(cm:SpecifiedColorMap) : PlotInfoMap = {
    project.inputFields.flatMap({fld1 =>
      project.inputFields.flatMap({fld2 =>
        if(fld1 < fld2) {
          Some(((fld1, fld2), 
            new ContinuousPlot(zoomDims.min(fld1), zoomDims.max(fld1),
                               zoomDims.min(fld2), zoomDims.max(fld2),
                               cm)))
        } else {
          None
        }
      })
    }).toMap
  }
  
  private def createAxes(position:Axis.Placement) = {
    val fields = position match {
      case Axis.HorizontalTop | Axis.HorizontalBottom => 
        sortedDims.init
      case Axis.VerticalLeft | Axis.VerticalRight => 
        sortedDims.tail
    }
    fields.map {fld => (fld, new Axis(position))} toMap
  }
}

