package tuner

import scala.actors.Actor
import scala.actors.Actor._

import tuner.gui.TimeDemoStatusWindow
import tuner.gui.ProjectViewer
import tuner.project.InputSpecification
import tuner.project.OutputSpecification
import tuner.project.ProjConfig
import tuner.project.Viewable

object TimeDemo {
  val maxDist = -math.log(Config.maxSampleSqDistance)

  def theta2Radius(theta:Float, d:Int) : Float = {
    math.sqrt(maxDist / theta / d.toFloat).toFloat
  }

  def radius2Theta(radius:Float, d:Int) : Float = {
    (maxDist / (radius*radius) / d.toFloat).toFloat
  }
}

class TimeDemoRunner(progWindow:TimeDemoStatusWindow, 
                     d:Int, n:Int, r:Float) extends Actor {
  def act = {
    (1 to Config.timeDemoRepeats).foreach {i =>
      runProject(d, n, r, i)
    }
  }

  def runProject(d:Int, n:Int, r:Float, i:Int) = {
    progWindow.dim = d
    progWindow.point = n
    progWindow.radius = r
    progWindow.trial = i

    val samples = randomSamples(d, n)
    val projConfig = ProjConfig(
      name = "d: %d n: %d r: %f i: %d".format(d, n, r, i),
      scriptPath = "/dev/null",
      inputs = (1 to d).map {dd => InputSpecification("x"+dd, 0f, 1f)} toList,
      outputs = List(OutputSpecification(Config.timeDemoOutputName, false)),
      ignoreFields = Nil,
      gpModels = List(fakeGpModel(samples, r)),
      buildInBackground = false,
      currentVis = VisInfo(
        currentSlice = (1 to d).map {dd => 
          SliceSpecification("x"+dd, scala.util.Random.nextFloat)
        } toList,
        currentZoom = (1 to d).map {dd => 
          ZoomSpecification("x"+dd, 0f, 1f)
        } toList,
        response1 = Some(Config.timeDemoOutputName),
        response2 = None,
        currentVis = Some("hyperslice"),
        currentMetric = "value",
        showSampleLine = false,
        showRegion = false
      ),
      currentRegion = Region.Default,
      history = None
    )
    val proj = new Viewable(projConfig, "/dev/null", samples)
    // Can't use the regular openProject functionality
    val projWindow = new ProjectViewer(proj)
    projWindow.open

    val sliders = projWindow.controlsTab.sliceSliders
    (0 until Config.timeDemoFocusChanges).foreach {j =>
      // pick a dimension
      val randDim = scala.util.Random.nextInt(d) + 1
      val randAmt = scala.util.Random.nextFloat

      val newVal = sliders("x"+randDim).value + randAmt
      if(newVal > 1) sliders("x"+randDim).value = newVal - 1
      else           sliders("x"+randDim).value = newVal
    }

    projWindow.close
  }

  def fakeGpModel(samples:Table, r:Float) : GpSpecification = {
    val d = samples.numFields - 1
    val n = samples.numRows
    val design = samples.data.map {tpl =>
      (1 to d).map {dd => tpl("x"+dd)}
    }
    val resps = List.fill(n)(1.0)
    val theta = TimeDemo.radius2Theta(r, d)

    val corMtx = new Jama.Matrix(n, n)
    val nrange = new scala.collection.parallel.immutable.ParRange(0 until n)
    nrange.foreach {i => 
      val xx1 = (1 to d).map {dd => samples.tuple(i)("x"+dd)}
      nrange.foreach {j =>
        val xx2 = (1 to d).map {dd => samples.tuple(j)("x"+dd)}
        val dist = xx1.zip(xx2).map({case (x1,x2) => math.pow(x1-x2, 2)}).sum
        // we can cheat because theta is constant across dimensions
        corMtx.set(i, j, math.exp(-d*theta * dist))
      }
    }
    val invCorMtx = corMtx.inverse

    GpSpecification(
      responseDim = Config.timeDemoOutputName,
      dimNames = (1 to d).map {dd => "x"+dd} toList,
      thetas = List.fill(d)(theta),
      alphas = List.fill(d)(2.0),
      mean = 0.0,
      sigma2 = 1.0,
      designMatrix = design.map {r => r.map(_.toDouble) toList} toList,
      responses = resps,
      invCorMtx.getArray.map {_.toList} toList)
  }

  def randomSamples(d:Int, n:Int) : Table = {
    val dimNames = (1 to d).map {dd => "x"+dd}
    val tbl = new Table
    (0 until n).foreach {r =>
      val vals = List.fill(d) {scala.util.Random.nextFloat}
      val output = List((Config.timeDemoOutputName, 1f))
      tbl.addRow(output ++ dimNames.zip(vals))
    }
    tbl
  }
}

