package tuner.gui

import scala.swing.ComboBox
import scala.swing.Label
import scala.swing.TablePanel
import scala.swing.event.SelectionChanged
import scala.swing.event.ValueChanged

import tuner.gui.event.NewDesignSelected
import tuner.gui.event.NewResponseSelected

import tuner.Sampler

/**
 * A panel allowing a user to select a file of already run samples to import
 */
class SampleImportPanel(newSamples:((Int, Sampler.Method) => Unit))
  extends TablePanel(2, 2) {

  val fileChooser = new PathPanel
  val valueSelector = new ComboBox(List("None")) {
    enabled = false
  }

  // Layouts
  layout(new Label("Sample File")) = (0,0)
  layout(new Label("Response Value")) = (0,1)
  layout(fileChooser) = (1,0)
  layout(valueSelector) = (1,1)

  // Event setup
  listenTo(fileChooser)
  listenTo(valueSelector)

  reactions += {
    case ValueChanged(`fileChooser`) => if(fileChooser.validPath) {
      publish(new NewDesignSelected(this))
    }
    case SelectionChanged(`valueSelector`) => 
      publish(new NewResponseSelected(this, valueSelector.selection.item))
  }

  def designPath : String = fileChooser.path
  def selectedResponse : Option[String] = if(valueSelector.enabled) {
    None
  } else {
    Some(valueSelector.selection.item)
  }
}
