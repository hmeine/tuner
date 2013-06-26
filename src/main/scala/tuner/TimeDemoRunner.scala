package tuner

import scala.actors.Actor
import scala.actors.Actor._

import org.jblas.{DoubleMatrix,FloatMatrix}

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

class TimeDemoRunner(d:Int, n:Int, r:Float) extends Actor {
  def act = {
    (1 to Config.timeDemoRepeats).foreach {i =>
      runProject(d, n, r, i)
    }
  }

  def runProject(d:Int, n:Int, r:Float, i:Int) = {
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
    val design = new DoubleMatrix(n, d)
    samples.data.zipWithIndex.foreach {case (tpl, r) =>
      (1 to d).foreach {dd => design.put(r, dd-1, tpl("x"+dd))}
    }
    val resps = List.fill(n)(1.0)
    val theta = TimeDemo.radius2Theta(r, d)

    val corMtx = DoubleMatrix.zeros(n, n)
    val rng1 = new scala.collection.parallel.immutable.ParRange(0 until n)
    rng1.foreach {i => 
      val xx1 = design.getRow(i)
      val rng2 = new scala.collection.parallel.immutable.ParRange(i until n)
      rng2.foreach {j =>
        val xx2 = design.getRow(j)
        val dist = xx1.squaredDistance(xx2)
        // we can cheat because theta is constant across dimensions
        val scaleDist = -d * theta * dist
        val corr = math.exp(scaleDist)
        corMtx.put(i, j, corr)
        corMtx.put(j, i, corr)
      }
    }
    val invCorMtx = org.jblas.Solve.solvePositive(corMtx, DoubleMatrix.eye(n))

    GpSpecification(
      responseDim = Config.timeDemoOutputName,
      dimNames = (1 to d).map {dd => "x"+dd} toList,
      thetas = List.fill(d)(theta),
      alphas = List.fill(d)(2.0),
      mean = 0.0,
      sigma2 = 1.0,
      designMatrix = design.toArray2.map {_.toList} toList,
      responses = resps,
      invCorMtx.toArray2.map {_.toList} toList)
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

