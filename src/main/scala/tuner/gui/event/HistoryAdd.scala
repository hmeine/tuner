package tuner.gui.event

import scala.swing.Component
import scala.swing.event.ComponentEvent

case class HistoryAdd(source:Component, slice:List[(String,Float)])
  extends ComponentEvent

