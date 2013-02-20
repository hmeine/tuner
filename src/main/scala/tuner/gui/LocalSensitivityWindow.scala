package tuner.gui

import scala.swing.Frame
import scala.swing.ScrollPane
import scala.swing.Table

import tuner.project.FunctionProject

class LocalSensitivityWindow(project:FunctionProject) extends Frame {

  val measurementTable = {
    val initialData:Array[Array[Any]] = computeMeasures.map {case (fld,v) => 
      (fld::decodeVals(v)).toArray.asInstanceOf[Array[Any]]
    }.toArray
    new Table(initialData, "Measure"::project.inputFields)
  }

  contents = new ScrollPane {
    contents = measurementTable
  }

  // Returns a list of measurement name/value pairs
  def computeMeasures:List[(String,Map[String,Float])] = 
    project.localSensitivities

  def updateView = {
    computeMeasures.zipWithIndex foreach {case ((m,vs),i) =>
      val dvs = decodeVals(vs)
      measurementTable.update(i, 0, m)
      dvs.zipWithIndex foreach {case (v,j) =>
        measurementTable.update(i, j+1, v)
      }
    }
  }

  private def decodeVals(vals:Map[String,Float]) : List[String] = {
    val f:PartialFunction[String,String] = {case x => ""}
    val mapper = vals andThen {x=>x.toString} orElse f
    project.inputFields.map {fld =>
      mapper(fld)
    }
  }

}
