package tuner.gui

import scala.swing.BorderPanel
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.Frame
import scala.swing.Label
import scala.swing.Orientation
import scala.swing.ScrollPane
import scala.swing.Swing
import scala.swing.TablePanel
import scala.swing.TextArea
import scala.swing.TextField
import scala.swing.event.ButtonClicked

import tuner.FunctionCompiler
import tuner.Region
import tuner.Tuner
import tuner.ViewInfo
import tuner.gui.event.ControlTableRowAdded
import tuner.project.FunctionProject
import tuner.project.FuncProjInfo
import tuner.project.InputSpecification
import tuner.project.ProjConfig

class NewFunctionProjectWindow extends Frame {

  title = "New Function Project"

  menuBar = new MainMenu

  resizable = true

  val projectNameField = new TextField
  val locationChooser = new PathPanel {
    fileSelector = FileChooser.loadDirectory _
  }

  val okButton = new Button("Ok")
  val cancelButton = new Button("Cancel")

  val inputDimTable = new ControlTable(List("Name", "Lower", "Upper")) {
    def controlRow = List(
      new TextField,
      new TextField,
      new TextField
    )
  }

  val funcMinField = new TextField
  val funcMaxField = new TextField

  val functionSourceField = new TextArea(48, 80) {
    border = Swing.TitledBorder(border, "Function Source")
  }

  val titlePanel = new BoxPanel(Orientation.Horizontal) {
    contents += Swing.HGlue
    contents += new Label("Enter function information")
    contents += Swing.HGlue
  }

  val contentPanel = new BoxPanel(Orientation.Vertical) {
    val projectInfoPanel = new TablePanel(List(90, TablePanel.Size.Fill),
                                          List(TablePanel.Size.Fill,TablePanel.Size.Fill)) {
      // Fields in the left column
      layout(new Label("Project Name")) = (0,0, TablePanel.HorizAlign.Right)
      layout(new Label("Save Location")) = (0,1, TablePanel.HorizAlign.Right)

      // Fields in the right column
      layout(projectNameField) = (1,0, TablePanel.HorizAlign.Full)
      layout(locationChooser) = (1,1, TablePanel.HorizAlign.Full)

      border = Swing.TitledBorder(border, "Project Info")
    }

    val inputsPanel = new ScrollPane {
      contents = inputDimTable

      border = Swing.TitledBorder(border, "Inputs")
    }

    val functionBoundsPanel = new TablePanel(List(90, TablePanel.Size.Fill),
                                          List(TablePanel.Size.Fill,TablePanel.Size.Fill)) {
      // Fields in the left column
      layout(new Label("Function Min")) = (0,0, TablePanel.HorizAlign.Right)
      layout(new Label("Function Max")) = (0,1, TablePanel.HorizAlign.Right)

      // Fields in the right column
      layout(funcMinField) = (1,0, TablePanel.HorizAlign.Full)
      layout(funcMaxField) = (1,1, TablePanel.HorizAlign.Full)

      border = Swing.TitledBorder(border, "Function Bounds")
    }

    contents += projectInfoPanel
    contents += Swing.HGlue
    contents += inputsPanel
    contents += Swing.HGlue
    contents += functionBoundsPanel
    contents += Swing.HGlue
    contents += functionSourceField
    contents += Swing.HGlue
  }

  val buttonPanel = new BoxPanel(Orientation.Horizontal) {
    contents += Swing.HGlue
    contents += cancelButton
    contents += okButton
  }

  contents = new BorderPanel {
    layout(titlePanel) = BorderPanel.Position.North
    layout(contentPanel) = BorderPanel.Position.Center
    layout(buttonPanel) = BorderPanel.Position.South
  }

  // Set up the interactions
  listenTo(okButton)
  listenTo(cancelButton)
  listenTo(inputDimTable)

  reactions += {
    case ButtonClicked(`okButton`) =>
      val proj = createNewProject
      Tuner.openProject(proj)
      close
    case ButtonClicked(`cancelButton`) =>
      close
      Tuner.top
    case ControlTableRowAdded(`inputDimTable`) =>
      this.pack
  }

  def inputDims : List[(String,Float,Float)] = {
    val controlValues = inputDimTable.controls.map {row =>
      val nameField = row(0).asInstanceOf[TextField]
      val minField = row(1).asInstanceOf[TextField]
      val maxField = row(2).asInstanceOf[TextField]
      if(nameField.text.length > 0 &&
         minField.text.length > 0 &&
         maxField.text.length > 0) {
        // Any conversion problems we ignore
        try {
          Some((nameField.text.trim, 
                minField.text.toFloat, 
                maxField.text.toFloat))
        } catch {
          case _ => None
        }
      } else {
        None
      }
    }
    controlValues.flatten.toList
  }

  def createNewProject : FunctionProject = {
    // create a fake config to pass down
    val projConfig = ProjConfig(
      projectNameField.text,
      inputDims map {case (fld,mn,mx) => InputSpecification(fld, mn, mx)},
      Nil,
      Nil,
      ViewInfo.Default,
      Region.Default,
      None,
      FuncProjInfo(
        functionSourceField.text,
        funcMinField.text.toFloat,
        funcMaxField.text.toFloat
      )
    )

    val func = FunctionCompiler.compile(functionSourceField.text)
    val np = new FunctionProject(projConfig, locationChooser.path, func) {
      def minValue(r:String) = funcMinField.text.toFloat
      def maxValue(r:String) = funcMaxField.text.toFloat
    }

    np.save()
    np
  }

}

