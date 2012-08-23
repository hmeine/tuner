package tuner.gui.R

import scala.swing.BorderPanel
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.Frame
import scala.swing.Label
import scala.swing.Orientation
import scala.swing.Swing
import scala.swing.event.ButtonClicked

import tuner.Rapp
import tuner.Tuner

object RJavaNotInstalledDialog extends Frame {

  val message = """<html>
    rJava is not installed<br/>
    <br/>
    Please install rJava before using Tuner.
    rJava can be installed by running "install.packages('rJava')"
    within R.<br/>
    </html>"""
  val messagePanel = new Label(message)
  val quitButton = new Button("Quit")

  listenTo(quitButton)

  reactions += {
    case ButtonClicked(`quitButton`) => Tuner.quit
  }

  defaultButton = quitButton

  contents = new BorderPanel {
    val buttonPanel = new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HGlue
      contents += quitButton
      contents += Swing.HGlue
    }
    layout(messagePanel) = BorderPanel.Position.Center
    layout(buttonPanel) = BorderPanel.Position.South
  }
}

