package tuner.gui

import scala.swing.BorderPanel
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.Frame
import scala.swing.Label
import scala.swing.Orientation
import scala.swing.Swing
import scala.swing.TablePanel

import tuner.Config

class TimeDemoStatusWindow extends Frame {

  title = "Time Demo"

  val cancelButton = new Button("Cancel")

  val dimProgressLabel = new Label(
    "0 of %d".format(Config.timeDemoDims.length))
  val pointProgressLabel = new Label(
    "0 of %d".format(Config.timeDemoPoints.length))
  val radiiProgressLabel = new Label(
    "0 of %d (???)".format(Config.timeDemoRadii.length))
  val trialProgressLabel = new Label(
    "0 of %d".format(Config.timeDemoRepeats))
  
  val titlePanel = new BoxPanel(Orientation.Horizontal) {
    contents += Swing.HGlue
    contents += new Label("Time Demo Status")
    contents += Swing.HGlue
  }

  val infoPanel = new TablePanel(List(90, TablePanel.Size.Fill),
                                 List.fill(4)(TablePanel.Size.Fill)) {
    layout(new Label("Dimensions")) = (0, 0, TablePanel.HorizAlign.Right)
    layout(dimProgressLabel)        = (1, 0, TablePanel.HorizAlign.Full)
    layout(new Label("Points"))     = (0, 1, TablePanel.HorizAlign.Right)
    layout(pointProgressLabel)      = (1, 1, TablePanel.HorizAlign.Full)
    layout(new Label("Radii"))      = (0, 2, TablePanel.HorizAlign.Right)
    layout(radiiProgressLabel)      = (1, 2, TablePanel.HorizAlign.Full)
    layout(new Label("Trial"))      = (0, 3, TablePanel.HorizAlign.Right)
    layout(trialProgressLabel)      = (1, 3, TablePanel.HorizAlign.Full)
  }

  val buttonPanel = new BoxPanel(Orientation.Horizontal) {
    contents += Swing.HGlue
    contents += cancelButton
  }

  contents = new BorderPanel {
    layout(titlePanel)  = BorderPanel.Position.North
    layout(infoPanel)   = BorderPanel.Position.Center
    layout(buttonPanel) = BorderPanel.Position.South
  }

  def dim = dimProgressLabel.text.split(" ", 2)(0) toInt
  def dim_=(d:Int) = {
    dimProgressLabel.text = "%d of %d".format(d, Config.timeDemoDims.length)
  }

  def point = pointProgressLabel.text.split(" ", 2)(0) toInt
  def point_=(p:Int) = {
    pointProgressLabel.text = "%d of %d".format(p, Config.timeDemoPoints.length)
  }

  def radius = radiiProgressLabel.text toFloat
  def radius_=(r:Float) = {
    radiiProgressLabel.text = "%f".format(r)
  }

  def trial = trialProgressLabel.text.split(" ", 2)(0) toInt
  def trial_=(n:Int) = {
    trialProgressLabel.text = "%d of %d".format(n, Config.timeDemoRepeats)
  }

}

