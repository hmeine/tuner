package tuner

object Color {

  def apply(argb:Int) = {
    val a = (argb >> 24) & 0xFF
    val r = (argb >> 16) & 0xFF
    val g = (argb >> 8) & 0xFF
    val b = argb & 0xFF
    new Color(r/255f, g/255f, b/255f, a/255f)
  }
  def apply(r:Float, g:Float, b:Float, a:Float) = new Color(r, g, b, a)

  implicit def c2Int(c:Color) : Int = c.toInt
  implicit def c2Floats(c:Color) : (Float, Float, Float) = (c.r, c.g, c.b)
}

class Color(val r:Float, val g:Float, val b:Float, val a:Float) {

  def red = r
  def green = g
  def blue = b
  def alpha = a

  def toInt : Int = {
    val aa = (a*255).toInt << 24  // Binary: 11111111000000000000000000000000
    val rr = (r*255).toInt << 16  // Binary: 00000000110011000000000000000000
    val gg = (g*255).toInt << 8   // Binary: 00000000000000001100110000000000
    val bb = (b*255).toInt
    aa | rr | gg | bb
  }

}

