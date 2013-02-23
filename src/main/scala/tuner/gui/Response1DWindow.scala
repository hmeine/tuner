package tuner.gui

import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.Frame

import tuner.project.FunctionProject

class Response1DWindow(project:FunctionProject, inputField:String) 
    extends Frame {

  title = inputField
  
  val respPanel = new ResponsePanel1D(project, inputField)
  contents = new BoxPanel(Orientation.Vertical) {
    contents += respPanel
  }

  def updateView = respPanel.loop = true
}

