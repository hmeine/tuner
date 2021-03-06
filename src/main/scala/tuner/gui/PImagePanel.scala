package tuner.gui

import processing.core.PImage

import tuner.Config

class PImagePanel(w:Int, h:Int) extends P5Panel(w, h) {
  
  private var _image:Option[PImage] = None

  def image:Option[PImage] = _image
  def image_=(i:Option[PImage]) : Unit = {
    _image = i
    loop
  }
  def image_=(i:PImage) : Unit = {
    image = Some(i)
    loop = true
  }

  override def setup = {
    loop = false
  }

  def draw = {
    loop = false

    applet.background(Config.backgroundColor)
    
    image.foreach {img =>
      val (imgWidth, imgHeight) = imageSize(img)
      imageMode(P5Panel.ImageMode.Center)
      image(img, w/2, h/2, imgWidth, imgHeight)
    }
  }

  def imageSize(img:PImage) : (Float,Float) = {
    val maxWidth = math.max(img.width, width)
    val maxHeight = math.max(img.height, height)

    // make sure to maintain aspect ratio when resizing
    val widthDiff = img.width - maxWidth
    val heightDiff = img.height - maxHeight

    // Figure out the most constrained factor and resize by that
    if(widthDiff > heightDiff) {
      (maxWidth, img.height * (maxWidth/img.width))
    } else {
      (img.width * (maxHeight/img.height), maxHeight)
    }
  }
}

