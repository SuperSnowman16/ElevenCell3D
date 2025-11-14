import java.awt.Color
import scala.math._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Colours {

    def IsDark(col:Color):Boolean = {
        val r = col.getRed()
        val gr = col.getGreen()
        val b = col.getBlue()
        val a = col.getAlpha()
        val brightness = (r * 0.299 + gr * 0.587 + b * 0.114 + a - 255).toInt
        return brightness < 70
    }


    def rgbToXyz(color: Color): Array[Double] = {
        def pivotRgb(c: Double): Double = {
          val v = c / 255.0
          if (v > 0.04045) Math.pow((v + 0.055) / 1.055, 2.4) else v / 12.92
        }
    
        val r = pivotRgb(color.getRed)
        val g = pivotRgb(color.getGreen)
        val b = pivotRgb(color.getBlue)
    
        // Observer = 2°, Illuminant = D65
        val x = r * 0.4124 + g * 0.3576 + b * 0.1805
        val y = r * 0.2126 + g * 0.7152 + b * 0.0722
        val z = r * 0.0193 + g * 0.1192 + b * 0.9505
    
        Array(x, y, z)
      }
    
      def xyzToLab(xyz: Array[Double]): Array[Double] = {
        def pivotXyz(t: Double): Double = {
          if (t > 0.008856) Math.cbrt(t) else (7.787 * t) + (16.0 / 116.0)
        }
    
        val refX = 0.95047 // D65 reference white
        val refY = 1.00000
        val refZ = 1.08883
    
        val x = pivotXyz(xyz(0) / refX)
        val y = pivotXyz(xyz(1) / refY)
        val z = pivotXyz(xyz(2) / refZ)
    
        val L = 116.0 * y - 16.0
        val a = 500.0 * (x - y)
        val b = 200.0 * (y - z)
    
        Array(L, a, b)
      }
    
      def rgbToLab(color: Color): Array[Double] = {
        val xyz = rgbToXyz(color)
        xyzToLab(xyz)
      }
    
      // Euclidean distance in CIELAB space
      def labDistance(lab1: Array[Double], lab2: Array[Double]): Double = {
        Math.sqrt(
          Math.pow(lab1(0) - lab2(0), 2) +
          Math.pow(lab1(1) - lab2(1), 2) +
          Math.pow(lab1(2) - lab2(2), 2)
        )
      }

  

  

  // Generate candidate colors from HSV grid
  def generateCandidates(numCandidates: Int): Seq[Color] = {
    val levels = Math.cbrt(numCandidates).ceil.toInt
    val candidates = ArrayBuffer[Color](Color.WHITE)
    for {
      h <- 0 until levels
      s <- 0 until levels
      v <- 1 until levels
      if candidates.size < numCandidates
    } {
      val hue = h.toFloat / levels
      val sat = s.toFloat / levels
      val value = v.toFloat / levels
      candidates += Color.getHSBColor(hue, sat, value)
    }
    
    candidates.toSeq
  }

  // Greedy max-min selection of distinct colors in LAB space
  def selectDistinctColors(candidates: Seq[Color], count: Int): Seq[Color] = {
    val selected = ArrayBuffer[Color]()
    val candidateLabs = candidates.map(rgbToLab)
    val selectedLabs = ArrayBuffer[Array[Double]]()

    // Pick first color randomly
    selected += candidates.head
    selectedLabs += candidateLabs.head

    while (selected.size < count) {
      val nextIndex = candidates.indices
        .filterNot(i => selected.contains(candidates(i)))
        .maxBy { i =>
          // minimum distance to any already selected color
          selectedLabs.map(lab => labDistance(candidateLabs(i), lab)).min
        }
      selected += candidates(nextIndex)
      selectedLabs += candidateLabs(nextIndex)
    }
    selected.toSeq
  }

  def DistinctColorArray(n:Int) : Array[Color] = {
    val candidates = generateCandidates(10*n)
    Random.shuffle(selectDistinctColors(candidates, n)).toArray
  }


  def labToColor(L: Double, a: Double, b: Double): Color = {
    // D65 reference white point
    val refX = 95.047
    val refY = 100.0
    val refZ = 108.883

    def fInv(t: Double): Double = {
      val t3 = t * t * t
      if (t3 > 0.008856) t3 else (t - 16.0 / 116.0) / 7.787
    }

    def linearToSrgb(c: Double): Int = {
      val v = if (c <= 0.0031308) 12.92 * c else 1.055 * pow(c, 1.0 / 2.4) - 0.055
      (max(0, min(1, v)) * 255).round.toInt
    }

    val fy = (L + 16.0) / 116.0
    val fx = a / 500.0 + fy
    val fz = fy - b / 200.0

    val x = refX * fInv(fx)
    val y = refY * fInv(fy)
    val z = refZ * fInv(fz)

    val rLin = ( 3.2406 * x - 1.5372 * y - 0.4986 * z) / 100.0
    val gLin = (-0.9689 * x + 1.8758 * y + 0.0415 * z) / 100.0
    val bLin = ( 0.0557 * x - 0.2040 * y + 1.0570 * z) / 100.0

    new Color(
      linearToSrgb(rLin),
      linearToSrgb(gLin),
      linearToSrgb(bLin)
    )
  }

  def labToColor(arr:Array[Double]) : Color = {
    labToColor(arr(0), arr(1), arr(2))
  }

  def main(args: Array[String]): Unit = {
    val numCandidates = 500
    val numColorsToSelect = 50

    val candidates = generateCandidates(numCandidates)
    val distinctColors = selectDistinctColors(candidates, numColorsToSelect)

    
  }



  def colorSpaceConversion(c:Color): Color = {
    val L = .2 + .6*c.getRed().toDouble/255
    val a = (c.getGreen().toDouble/255 - .5)*.5
    val b = (c.getBlue().toDouble/255 - .5)*.5
    // Step 1: Convert OKLab to LMS
    val l_ = L + 0.3963377774 * a + 0.2158037573 * b
    val m_ = L - 0.1055613458 * a - 0.0638541728 * b
    val s_ = L - 0.0894841775 * a - 1.2914855480 * b

    // Step 2: Nonlinear to linear
    val l = Math.pow(l_, 3)
    val m = Math.pow(m_, 3)
    val s = Math.pow(s_, 3)

    // Step 3: LMS to linear RGB
    val R_lin =  4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s
    val G_lin = -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s
    val B_lin = -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s

    // Step 4: Linear to gamma-corrected sRGB
    def linearToSRGB(c: Double): Double = {
      if (c <= 0.0031308) 12.92 * c
      else 1.055 * Math.pow(c, 1.0 / 2.4) - 0.055
    }

    val R = linearToSRGB(R_lin)
    val G = linearToSRGB(G_lin)
    val B = linearToSRGB(B_lin)

    // Step 5: Clamp to [0, 255]
    def clampToByte(v: Double): Int = {
      val clamped = Math.max(0.0, Math.min(1.0, v))
      (clamped * 255.0 + 0.5).toInt  // Add 0.5 for rounding
    }

    new Color(
      clampToByte(R),
      clampToByte(G),
      clampToByte(B)
    )
  }

  def mean(arr:Array[Double]) : Double = {
    arr.sum / arr.length
  }
  def averageColor(arr : Array[Color]) : Color = {
    labToColor(arr.map(rgbToLab).transpose.map(mean))
  }
}

