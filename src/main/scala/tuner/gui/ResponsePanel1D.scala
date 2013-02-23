package tuner.gui

import tuner.Color
import tuner.Config
import tuner.geom.Rectangle
import tuner.gui.widgets.Axis
import tuner.project.FunctionProject

class ResponsePanel1D(project:FunctionProject, inputField:String)
    extends P5Panel(500, 500, P5Panel.Java2D) {

  val xAxisTicks:List[Float] = List(project.inputs.min(inputField),
                                    project.inputs.mean(inputField),
                                    project.inputs.max(inputField))
  val yAxisTicks:List[Float] = List(project.minValue("y"), 
                                    project.mean, 
                                    project.maxValue("y"))
  val xAxis = new Axis(Axis.HorizontalBottom)
  val yAxis = new Axis(Axis.VerticalLeft)

  // Bounds
  val plotWidth = 500 - (Config.plotSpacing * 2) - Config.axisSize
  val plotHeight = 500 - (Config.plotSpacing * 2) - Config.axisSize
  val yAxisBounds = Rectangle((Config.plotSpacing, Config.plotSpacing),
                              Config.axisSize, plotHeight)
  val plotBounds = Rectangle((yAxisBounds.maxX, Config.plotSpacing), 
                             plotWidth, plotHeight)
  val xAxisBounds = Rectangle((plotBounds.minX, plotBounds.maxY),
                              plotWidth, Config.axisSize)

  override def setup = {
    loop = false
    super.setup
  }

  def draw = {
    loop = false
    applet.background(Config.backgroundColor)

    // Draw the main plot
    drawPlot

    // Draw the sensitivity measures
    drawGradients

    // Draw the axes
    yAxis.draw(this, yAxisBounds.minX, yAxisBounds.minY,
                     yAxisBounds.width, yAxisBounds.height,
                     "y", yAxisTicks)
    xAxis.draw(this, xAxisBounds.minX, xAxisBounds.minY,
                     xAxisBounds.width, xAxisBounds.height,
                     inputField, xAxisTicks)
  }

  def mapx(x:Float) = P5Panel.map(x, project.inputs.min(inputField), 
                                     project.inputs.max(inputField),
                                     plotBounds.minX, 
                                     plotBounds.maxX)

  def mapy(y:Float) = P5Panel.map(y, project.minValue("y"),
                                     project.maxValue("y"),
                                     plotBounds.maxY,
                                     plotBounds.minY)

  def invMapx(xx:Float) = P5Panel.map(xx, plotBounds.minX,
                                          plotBounds.maxX,
                                          project.inputs.min(inputField), 
                                          project.inputs.max(inputField))

  def invMapy(yy:Float) = P5Panel.map(yy, plotBounds.maxY,
                                          plotBounds.minY,
                                          project.minValue("y"),
                                          project.maxValue("y"))

  def drawPlot = {
    val remPt = project.viewInfo.currentSlice filterKeys {_ != inputField} toList
    val steps = 250
    var xVal = project.inputs.min(inputField)
    val step = (project.inputs.max(inputField)-project.inputs.min(inputField)) / steps.toFloat

    strokeWeight(1)
    stroke(255)

    var lastXX = mapx(xVal)
    var lastYY = mapy(project.value((inputField, xVal)::remPt, "y"))
    xVal += step

    while(xVal < project.inputs.max(inputField)) {
      val xx = mapx(xVal)
      val yy = mapy(project.value((inputField, xVal)::remPt, "y"))
      line(lastXX, lastYY, xx, yy)
      xVal += step
      lastXX = xx
      lastYY = yy
    }
    val xx = plotBounds.maxX
    val yy = mapy(
      project.value((inputField, project.inputs.max(inputField))::remPt, "y"))
    line(lastXX, lastYY, xx, yy)
  }

  def drawGradients = {
    val gradMeasures = List(
      ("gradient", project.centralDiffGradient _),
      ("value-normed gradient", project.valueNormedGradient _),
      ("mean-normed gradient", project.meanNormedGradient _),
      ("variance-normed gradient", project.varianceNormedGradient _)
    )

    gradMeasures.foreach {case (measureStr, measureF) =>
      drawGradient(measureF(project.viewInfo.currentSlice.toList, inputField),
                   Color(Config.sensitivityColors(measureStr), 0.8f))
    }
  }

  def drawGradient(slope:Float, color:Color) = {
    val pixels = 30
    val x = project.viewInfo.currentSlice(inputField)
    val y = project.value(project.viewInfo.currentSlice.toList, "y")

    val minXX = mapx(x) - pixels
    val maxXX = mapx(x) + pixels

    val minX = invMapx(minXX)
    val maxX = invMapx(maxXX)

    val minY = y + slope * (minX - x)
    val maxY = y + slope * (maxX - x)

    strokeWeight(2)
    stroke(color)
    line(minXX, mapy(minY), maxXX, mapy(maxY))
  }
}

