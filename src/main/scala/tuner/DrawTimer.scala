package tuner

import java.io.{BufferedWriter,FileWriter}

import collection.mutable.MutableList

/**
 * Records statistics on how long things take to draw. 
 * Then we can predict how long drawing will take.
 *
 * For predicting the timing we need to record the following items:
 * * number of sample points to process
 * * the radii of the gaussian kernels
 * * the extents of the various dimensions (to normalize the radii)
 * * dimensionality of the data
 * * the time it took to draw
 */
object DrawTimer {

  implicit def int2Timing(i:Int) : Timing = Nanos(i)
  sealed trait Timing {
    def +(t2:Timing) : Timing = {
      // Just convert everything to nanos
      val x1 = this match {
        case Millis(m) => m * 1000000L
        case Nanos(n)  => n
      }
      val x2 = t2 match {
        case Millis(m) => m * 1000000L
        case Nanos(n)  => n
      }
      Nanos(x1 + x2)
    }
    def -(t2:Timing) : Timing = {
      // Just convert everything to nanos
      val x1 = this match {
        case Millis(m) => m * 1000000L
        case Nanos(n)  => n
      }
      val x2 = t2 match {
        case Millis(m) => m * 1000000L
        case Nanos(n)  => n
      }
      Nanos(x1 - x2)
    }

    def toSeconds:Double

    override def toString = toSeconds.toString
  }
  case class Millis(m:Long) extends Timing {
    def toSeconds = m / 1e3d
  }
  case class Nanos(n:Long) extends Timing {
    def toSeconds = n / 1e9d
  }

  // A tuple of type (lower dim bound, upper dim bound, radius)
  type TimingRadii = (Float,Float,Float)

  // The time to draw an individual hyperslice plot matrix
  //val drawTimes = new MutableList[(Int,Iterable[TimingRadii],Timing)]
  lazy val drawTimes = {
    val r = new BufferedWriter(new FileWriter("all_times.csv", true))
    r.write("timestamp,GPU,total points,cpu time (sec),gpu time (sec),points passed,pixels,min1,max1,radius1\n")
    r
  }

  // The time to draw all the static information that doesn't depend
  // on what hyperslice view we're drawing
  //val staticTimes = new MutableList[Timing]
  lazy val staticTimes = {
    val r = new BufferedWriter(new FileWriter("static_times.csv", true))
    r.write("timestamp,static time (sec)\n")
    r
  }

  /*
  lazy val outProcs = new BufferedWriter(new FileWriter("proc_list.out", true))
  lazy val outTemps = {
    val r = new BufferedWriter(new FileWriter("temp_list.csv", true))
    val temps = new ProcessBuilder("/Applications/TemperatureMonitor.app/Contents/MacOS/tempmonitor", "-c", "-th").start
    temps.waitFor
    val tempList = new java.io.BufferedReader(new java.io.InputStreamReader(temps.getInputStream))
    var line:String = null
    line = tempList.readLine
    while(line != null) {
      r.write(line + "\n")
      line = tempList.readLine
    }
    r
  }
  lazy val outScreen = new BufferedWriter(new FileWriter("screen_list.out", true))
  */

  /**
   * A utility function to return the time to run a block of code
   */
  def timed(log:Boolean)(block: => Unit) : Timing = {
    val startTime = System.nanoTime
    block
    val endTime = System.nanoTime
    if(log) println("t1: " + startTime + " t2: " + endTime)
    val time = Nanos(endTime-startTime)
    if(log) println("logged time: " + time)
    time
  }

  def timed(block: => Unit) : Timing = timed(false)(block)

  def printTime[R](name:String)(block: => R) : R = {
    val startTime = System.nanoTime
    val x:R = block
    val endTime = System.nanoTime
    val time = Nanos(endTime-startTime)
    if(name != "") {
      println(name + ": " + time + " sec")
    } else {
      println(time + " sec")
    }
    x
  }

  def saveAll = {
    drawTimes.close
    staticTimes.close
    /*
    outProcs.close
    outTemps.close
    outScreen.close
    */
  }

  /**
   * add a timing for drawing code that doesn't depend on the response surface
   */
  def addStaticTiming(time:Timing) = {
    staticTimes.write((new java.util.Date).toString + ",")
    staticTimes.write(time.toString)
    staticTimes.write("\n")
  }

  /**
   * add a drawing timing for spherical kernels in (0,1) dimension spaces
   */
  def addSphericalTiming(gpuName:String,
                         totalPoints:Int, radius:Float, dims:Int, 
                         pointsPassed:Int, pixels:Float, 
                         cpuTime:Timing, gpuTime:Timing) =
    addElipticalTiming(gpuName, totalPoints, 
                       List.fill(dims)((0, 1, radius)), 
                       pointsPassed,
                       pixels, 
                       cpuTime, 
                       gpuTime)

  /**
   * add a drawing timing for eliptical kernels
   */
  def addElipticalTiming(gpuName:String,
                         totalPoints:Int, radii:Iterable[TimingRadii], 
                         pointsPassed:Int, pixels:Float,
                         cpuTime:Timing, gpuTime:Timing) = {
    val ts = new java.util.Date
    drawTimes.write(ts.toString + ",")
    drawTimes.write(gpuName + ",")
    drawTimes.write(totalPoints + ",")
    drawTimes.write(cpuTime.toString + ",")
    drawTimes.write(gpuTime.toString + ",")
    drawTimes.write(pointsPassed + ",")
    drawTimes.write(pixels.toString + ",")
    drawTimes.write(
      radii.map({case (mn,mx,r) => mn + "," + mx + "," + r}).mkString(",")
    )
    drawTimes.write("\n")

    // Write out a ps trace
    /*
    val ps = new ProcessBuilder("ps", "-Av").start
    ps.waitFor
    val procList = new java.io.BufferedReader(new java.io.InputStreamReader(ps.getInputStream))
    var line:String = null
    line = procList.readLine
    while(line != null) {
      val outLine = ts.toString + " " + line
      outProcs.write(outLine + "\n")
      line = procList.readLine
    }
    */

    /*
    // Also write out the current machine temps
    var line:String = null
    val temps = new ProcessBuilder("/Applications/TemperatureMonitor.app/Contents/MacOS/tempmonitor", "-c", "-tv").start
    temps.waitFor
    val tempList = new java.io.BufferedReader(new java.io.InputStreamReader(temps.getInputStream))
    line = tempList.readLine
    while(line != null) {
      outTemps.write(line + "\n")
      line = tempList.readLine
    }

    // Also write out whether the screen is awake or not
    val screen = new ProcessBuilder("/Users/tom/Projects/tuner.dev/screensleep.sh").start
    screen.waitFor
    val screenInfo = new java.io.BufferedReader(new java.io.InputStreamReader(screen.getInputStream))
    line = screenInfo.readLine
    while(line != null) {
      val outLine = ts.toString + " " + line
      outScreen.write(outLine + "\n")
      line = screenInfo.readLine
    }
    */
  }

}

