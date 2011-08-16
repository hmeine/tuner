package tuner

//import org.rosuda.REngine.JRI.JRIEngine
//import org.rosuda.REngine.RList
//import org.rosuda.REngine.REXP
//import org.rosuda.JRI.RConsoleOutputStream

import org.rosuda.JRI.Rengine
import org.rosuda.JRI.REXP
import org.rosuda.JRI.RList
import org.rosuda.JRI.RVector
import org.rosuda.JRI.RMainLoopCallbacks

import java.io.PrintStream

import tuner.error._

object R {

  val MacRHome = "/Library/Frameworks/R.framework/Resources"

  // Any special arguments to R go in this array
  val RARGS = List("--no-save", "--slave")
  val RequiredPackages:Seq[String] = List("mlegp", "lhs", "labeling")

  print("loading R library...")
  try {
    System.loadLibrary("jri")
  } catch {
    case se:SecurityException => 
      throw new RInitException("Could not load jri due to security", se)
    case le:UnsatisfiedLinkError => 
      throw new MissingJriException(le)
    case ne:NullPointerException => 
      throw new RInitException("Could not load jri due to null pointer", ne)
    case e:Exception => 
      throw new RInitException("Could not load jri. unknown reason", e)
  }
  println("done")

  print("initializing R...")
  val engine = new Rengine(RARGS.toArray, false, new RTextConsole)
  if (!engine.waitForR()) {
    // A common cause is not setting the R_HOME variable
    if(System.getenv("R_HOME") == null) {
      throw new RInitException("Cannot load R. R_HOME not set.")
    } else {
      throw new RInitException("Cannot load R")
    }
  }
  println("done")

  ensurePackages

  //System.setOut(new PrintStream(new RConsoleOutputStream(engine.getRni, 0)))
  //System.setErr(new PrintStream(new RConsoleOutputStream(engine.getRni, 1)))

  /**
   * Returns the path to the R installation
   */
  def rPath : Option[String] = OS.detect match {
    case OS.Mac =>
      val home = new java.io.File(MacRHome)
      if(home.exists && home.isDirectory) Some(MacRHome)
      else                                None
    case OS.Win =>
      val regKey = "HKLM\\Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\"
      val programs = WindowsRegistry.readRegistry(regKey, "")
      println(programs)
      None
  }

  /**
   * Make sure all the required packages are installed
   */
  def ensurePackages = RequiredPackages.foreach {pkg => installPackage(pkg)}

  def missingPackages = RequiredPackages.filter {pkg => !hasPackage(pkg)}

  def hasPackage(pkg:String) : Boolean = {
    val checkCmd = "is.element('%s', installed.packages()[,1])"
    runCommand(checkCmd.format(pkg)).asBool.isTRUE
  }

  def installPackage(pkg:String) = {
    val instCmd = "install.packages('%s', repos='http://cran.r-project.org')"

    if(!hasPackage(pkg)) {
      println("installing %s".format(pkg))
      runCommand(instCmd.format(pkg))
    }
  }

  def runCommand(cmd:String) : REXP = {
    //println(cmd)
    //val rcmd = engine.parse(cmd, false)
    //val res = engine.eval(rcmd, null, false)
    val res = engine.eval(cmd)
    /*
    if(res.isNull) {
      throw new Exception("ERROR: " + cmd + " failed")
    }
    */
    res
  }

  /*
  def toREXP(values:Array[Double]) = {
    new REXP(values))
  }
  */

  def setVar(exp:REXP, sym:String) = {
    engine.assign(sym, exp)
  }

  def quit() = engine.end

}

