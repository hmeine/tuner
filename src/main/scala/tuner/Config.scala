package tuner

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream

import tuner.util.Prefs

object Config {
  
  def recentProjects : Set[String] = Prefs.list("recentProjects") toSet
  def recentProjects_=(rp:Set[String]) = {
    Prefs.saveList("recentProjects", rp.toList)
  }
  
  val projConfigFilename = "config.json"
  val sampleFilename = "samples.csv"
  val designFilename = "responses.csv"

  val gpRScript = getClass.getResource("/sparkle.R").getPath

  val errorField = "stddev"
  val gainField = "estgain"

  val estimateSampleDensity = 20
}

