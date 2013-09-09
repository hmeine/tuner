package tuner.test

import org.scalacheck._
import org.scalatest.FunSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.matchers.ShouldMatchers._

import tuner.Config
import tuner.DimRanges
import tuner.GpModel
import tuner.Region
import tuner.Table
import tuner.TimeDemoRunner
//import tuner.{SliceSpecification, VisInfo, ZoomSpecification}
//import tuner.project.{InputSpecification, OutputSpecification, ProjConfig}
import tuner.project.Viewable

class GpModelSpec extends FunSuite with GeneratorDrivenPropertyChecks {

  val dimGen = Gen.choose(2, 10)
  val radiusGen = Gen.choose(0f, 0.5f) suchThat (_ > 0)
  //val pointGen = (Gen.choose(0f, 1f), Gen.choose(0f, 1f))
  val pointGen = for {
    x1 <- Gen.choose(0f, 1f)
    x2 <- Gen.choose(0f, 1f)
  } yield (x1, x2)
  val points2dGen = Gen.listOf1[(Float,Float)](pointGen)
  val largePoints2dGen = for {
    n <- Gen.choose(500, 5000)
    pts <- Gen.listOfN[(Float,Float)](n, pointGen)
  } yield (pts)

  test("a point in the center of the view should have full fragments") {
    //val d = 3
    //val r = 0.25f
    forAll(dimGen, radiusGen) {(d:Int, r:Float) =>
      whenever(d >= 2 && r > 0 && r <= 0.5) {
        val samples = new Table
        samples.addRow(("y", 1f) :: ((1 to d).map {i=>("x"+i, 0.5f)} toList))
        val gp = GpModel.fromJson(TimeDemoRunner.fakeGpModel(samples, r))
        val focusPt = (1 to d).map {i => ("x"+i, 0.5f)} toList
        val rng = new DimRanges((1 to d).map {i => ("x"+i, (0f, 1f))} toMap)
    
        val frags = gp.fragmentsDrawn(focusPt, rng)
  
        val plots = d * (d-1) / 2
        val area = 4 * r * r
        val correctFrags = area * plots
        frags should be (correctFrags plusOrMinus 1e-5f)
      }
    }
  }

  test("the size of impact of offset quads") {
    forAll(radiusGen, points2dGen) {(r:Float, pts:List[(Float,Float)]) =>
      whenever(r > 0 && r <=0.5 && pts.length > 0) {
        val samples = new Table
        pts.foreach {case(x1,x2) =>
          samples.addRow(List(("y", 1f), ("x1", x1), ("x2", x2)))
        }
        val gp = GpModel.fromJson(TimeDemoRunner.fakeGpModel(samples, r))
        val focusPt = List(("x1", 0.5f), ("x2", 0.5f))
        val rng = new DimRanges(Map("x1" -> (0f, 1f), "x2" -> (0f, 1f)))
    
        val frags = gp.fragmentsDrawn(focusPt, rng)
  
        val area = pts.foldLeft(0f) {case(ttl, pt) =>
          val minX = math.max(pt._1 - r, 0f)
          val maxX = math.min(pt._1 + r, 1f)
          val minY = math.max(pt._2 - r, 0f)
          val maxY = math.min(pt._2 + r, 1f)
          ttl + (maxX - minX) * (maxY - minY)
        }
        frags should be (area plusOrMinus 1e-5f)
      }
    }
  }

  /*
  this is commented out because i don't know how to do relative comparisons
  of the average easily given the convergence rate
  test("the average size of impact of a 2D quad") {
    forAll(radiusGen, largePoints2dGen) {(r:Float, pts:List[(Float,Float)]) =>
      whenever(r > 0 && r <=0.25 && pts.length >= 500) {
        val samples = new Table
        pts.foreach {case(x1,x2) =>
          samples.addRow(List(("y", 1f), ("x1", x1), ("x2", x2)))
        }
        val gp = GpModel.fromJson(TimeDemoRunner.fakeGpModel(samples, r))
        val focusPt = List(("x1", 0.5f), ("x2", 0.5f))
        val rng = new DimRanges(Map("x1" -> (0f, 1f), "x2" -> (0f, 1f)))
    
        val frags = gp.fragmentsDrawn(focusPt, rng)
  
        val expFrags = 4*math.pow(r, 2) - 4*math.pow(r, 3) + math.pow(r, 4)
        val empFrags = frags / pts.length
        empFrags should be (expFrags.toFloat plusOrMinus 1e-2f)
      }
    }
  }
  */

  /*
  test("the expected size of a 3D quad in the center") {
    forAll(radiusGen) {r:Float =>
      whenever(r > 0 && r <= 0.5) {
        val samples = new Table
        samples.addRow(List(("y", 1f), ("x1", 0.5f), ("x2", 0.5f), ("x3", 0.5f)))
        val gp = GpModel.fromJson(TimeDemoRunner.fakeGpModel(samples, r))
        val rng = new DimRanges(Map("x1" -> (0f, 1f), 
                                    "x2" -> (0f, 1f),
                                    "x3" -> (0f, 1f)))
        val focusPt = List(("x1", 0.5f), ("x2", 0.5f))
    
        val NN = 101
        val ttlFrags = (-(NN-1)/2 until (NN-1)/2).foldLeft(0f) {case(ttl, i) =>
          val t = i * (r / (NN-1) * 2)
          val u = math.sqrt(r*r - t*t) toFloat
          val focusPt = List(("x1", 0.5f), ("x2", 0.5f), ("x3", t))
          val frags = gp.fragmentsDrawn(focusPt, rng)

          ttl + List(r, r, u).foldLeft(0f) {case(a,uu) =>
            val minX = math.max(0.5f - uu, 0f)
            val maxX = math.min(0.5f + uu, 1f)
            val minY = math.max(0.5f - uu, 0f)
            val maxY = math.min(0.5f + uu, 1f)
            a + (maxX - minX) * (maxY - minY)
          }
        }

        val predFrags = (2 * ((8f/3) * math.pow(r, 3) - 
                              (3f*math.Pi/4) * math.pow(r, 4) +
                              (8f/15) * math.pow(r, 5))).toFloat
        val pf2 = (predFrags + 2 * (4 * math.pow(r, 2))).toFloat
        (ttlFrags/NN) should be (pf2 plusOrMinus 1e-5f) 
        
      }
    }
  }
  */

}

