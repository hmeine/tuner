package tuner.gui

import tuner.gui.event.SliceChanged
import tuner.project.SimViewable

class CandidatesPanel(project:SimViewable) 
    extends TableSelectionPanel(project, project.candidates) {
  
  override def updateTable = {
    super.updateTable
    dataTable.selection.rows += 0
  }
}

