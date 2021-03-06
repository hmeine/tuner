package tuner.gui.R

import scala.swing.BorderPanel
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.Frame
import scala.swing.Label
import scala.swing.Orientation
import scala.swing.ProgressBar
import scala.swing.Swing
import scala.swing.event.ButtonClicked

import tuner.Rapp
import tuner.Tuner

abstract class InstallPackageDialog(packages:Seq[String]) extends Frame {

  val installPackage:String=>Unit

  val installMessage = {
    val words = if(packages.length > 1) ("are", "them")
                else                    ("is", "it")
    packages.reduceLeft(_ + ", " + _) + " " + words._1 +
      " required and not installed.  Would you like to install " + 
      words._2 + "?"
  }
  val installMessagePanel = new Label(installMessage)
  val installStatusPanel = new Label("")
  val quitButton = new Button("Quit")
  val installButton = new Button("Install")
  val cancelButton = new Button("Cancel")
  val restartButton = new Button("Restart Tuner")

  // == Various stages of this dialog ==

  // Asks the user if they want to install the packages
  val messageStage = new BorderPanel {
    val buttonPanel = new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HGlue
      contents += quitButton
      contents += installButton
      contents += Swing.HGlue
    }
    layout(installMessagePanel) = BorderPanel.Position.Center
    layout(buttonPanel) = BorderPanel.Position.South
  }

  // displays which package is being installed
  val installStage = new BorderPanel {
    val buttonPanel = new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HGlue
      contents += cancelButton
      contents += Swing.HGlue
    }
    val statusPanel = new BoxPanel(Orientation.Vertical) {
      contents += new ProgressBar {
        indeterminate = true
      }
      contents += installStatusPanel
    }

    layout(statusPanel) = BorderPanel.Position.Center
    layout(buttonPanel) = BorderPanel.Position.South
  }

  // asks for restart (which is the only option)
  val successStage = new BorderPanel {
    val msgPanel = new Label("Packages are installed. Restart?")
    val buttonPanel = new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HGlue
      contents += restartButton
      contents += Swing.HGlue
    }
    layout(msgPanel) = BorderPanel.Position.Center
    layout(buttonPanel) = BorderPanel.Position.South
  }

  listenTo(quitButton)
  listenTo(installButton)
  listenTo(cancelButton)
  listenTo(restartButton)

  reactions += {
    case ButtonClicked(`quitButton`) => 
      Tuner.quit
    case ButtonClicked(`cancelButton`) => 
      Tuner.quit
    case ButtonClicked(`installButton`) => 
      contents = installStage
      defaultButton = None
      packages.foreach {pkg =>
        installStatusPanel.text = "Installing " + pkg + "..."
        installPackage(pkg)
      }
      contents = successStage
      defaultButton = restartButton
    case ButtonClicked(`restartButton`) =>
      Tuner.quit
  }

  contents = messageStage
  defaultButton = installButton
}

