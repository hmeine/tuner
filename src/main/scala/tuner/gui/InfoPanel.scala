package tuner.gui

import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.Panel
import scala.swing.ScrollPane
import scala.swing.Swing
import scala.swing.Table

import tuner.Project

import java.awt.Dimension
import javax.swing.table.AbstractTableModel

class InfoPanel(project:Project) extends BoxPanel(Orientation.Vertical) {

  val infoTable = {
    val columnNames = project.inputFields ++ 
                      project.responseFields.flatMap({rf =>
                        List(rf, rf + " Error", rf + " Gain")
                      })
    val initialData:Array[Array[Any]] = Array(
      "Estimate" +: Array.fill(columnNames.length)(""),
      "Nearest Sample" +: Array.fill(columnNames.length)(""))
    new Table(initialData, "" :: columnNames)
  }

  val imagePanel = new Panel {
    maximumSize = new Dimension(300, 150)
    preferredSize = new Dimension(300, 150)
  }

  contents += new ScrollPane {
    contents = infoTable
    maximumSize = new Dimension(800, 70)
    preferredSize = new Dimension(800, 70)
  }
  contents += Swing.VGlue
  contents += imagePanel
  contents += Swing.VGlue

  def updateTable = {
    val slice = project.currentSlice
    val closestSample = project.closestSample(slice.toList).toMap
    val estimates = project.estimatePoint(slice.toList)

    // Update the independent dimensions first
    project.inputFields.zipWithIndex.foreach {case (fld, i) =>
      infoTable.update(0, i+1, slice.getOrElse(fld, ""))
      infoTable.update(1, i+1, closestSample.getOrElse(fld, ""))
    }

    // Also update the response fields
    project.responseFields.zipWithIndex.foreach {case (fld, i) =>
      val col = 1 + project.inputFields.length + 3 * i
      val (est, err, gain) = estimates.getOrElse(fld, ("","",""))
      infoTable.update(0, col, est)
      infoTable.update(0, col+1, err)
      infoTable.update(0, col+2, gain)
      infoTable.update(1, col, closestSample.getOrElse(fld, ""))
    }
  }

}
