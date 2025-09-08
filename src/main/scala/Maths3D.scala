import com.badlogic.gdx.math.Vector3
import java.lang.Math._
object Maths3D {
	case class Sphere(center: Vector3, radius: Float){
		def inv(pt:Vector3) : Vector3 = sphericalInversion(pt, this).get
	}

	val unitSphere = new Sphere(new Vector3, 1)

	def circumsphere(p1: Vector3, p2: Vector3, p3: Vector3, p4: Vector3): Option[Sphere] = {
		val A = Array(
			Array((p2.x - p1.x).toDouble, (p2.y - p1.y).toDouble, (p2.z - p1.z).toDouble),
			Array((p3.x - p1.x).toDouble, (p3.y - p1.y).toDouble, (p3.z - p1.z).toDouble),
			Array((p4.x - p1.x).toDouble, (p4.y - p1.y).toDouble, (p4.z - p1.z).toDouble)
		)

		val b = Array(
			0.5 * ((p2.len2() - p1.len2()).toDouble),
			0.5 * ((p3.len2() - p1.len2()).toDouble),
			0.5 * ((p4.len2() - p1.len2()).toDouble)
		)

		try {
			val centerArr = solve3x3(A, b)
			val center = new Vector3(centerArr(0).toFloat, centerArr(1).toFloat, centerArr(2).toFloat)
			val radius = center.dst(p1) // distance from center to any vertex
			Some(Sphere(center, radius))
		} catch {
			case _: Exception =>
			None // no unique circumsphere (coplanar or degenerate points)
		}
		}

		def solve3x3(A: Array[Array[Double]], b: Array[Double]): Array[Double] = {
		val M = A.map(_.clone)
		val x = b.clone
		val n = 3

		for (i <- 0 until n) {
			// pivot
			var max = i
			for (j <- i+1 until n if Math.abs(M(j)(i)) > Math.abs(M(max)(i))) max = j
			val tmpRow = M(i); M(i) = M(max); M(max) = tmpRow
			val tmpVal = x(i); x(i) = x(max); x(max) = tmpVal

			// eliminate
			for (j <- i+1 until n) {
			val f = M(j)(i) / M(i)(i)
			for (k <- i until n) M(j)(k) -= f * M(i)(k)
			x(j) -= f * x(i)
			}
		}

		// back-substitution
		val res = Array.ofDim[Double](n)
		for (i <- (0 until n).reverse) {
			res(i) = (x(i) - (i+1 until n).map(k => M(i)(k) * res(k)).sum) / M(i)(i)
		}
		res
	}

	def sphericalInversion(p: Vector3, s: Sphere): Option[Vector3] = {
		val v = new Vector3(p).sub(s.center)
		val dist2 = v.len2()

		if (dist2 == 0f) {
			None // cannot invert the sphere center
		} else {
			val scale = (s.radius * s.radius) / dist2
			Some(new Vector3(v).scl(scale).add(s.center))
		}
	}

	def sphereMirror(p:Vector3, s:Sphere) : Vector3 = sphericalInversion(p, s).get

	def asinh(x:Double) : Double = {
        log(x + sqrt(x*x +1))
    }
    def acosh(x:Double) : Double = {
        require(x >= 1)
        log(x + sqrt(x*x -1))
    } 
    def atanh(x:Double) : Double = {
		if(abs(x) >= 1){println(x)}
        require(abs(x) < 1)
        log((1+x)/(1-x))/2
    }

    def mod(x:Int,y:Int):Int = {
        require(y > 0)
        if (x % y >= 0){
            x%y
        }else{
            (x%y)+y
        }
    }


	// Möbius operations using Vector3
	object Mobius {

	/** Möbius addition: u ⊕ v */
		def mobiusAdd(u: Vector3, v: Vector3): Vector3 = {
			val u2 = u.len2()
			val v2 = v.len2()
			val uv = u.dot(v)

			val numerator = u.cpy().scl(1 + 2 * uv + v2).add(v.cpy().scl(1 - u2))
			val denominator = 1 + 2 * uv + u2 * v2
			numerator.scl(1.0f / denominator.toFloat)
		}

		/** Möbius scalar multiplication: r ⊗ x */
		def mobiusScalarMultiply(r: Double, x: Vector3): Vector3 = {
			val xNorm = x.len()
			if (xNorm == 0) x.cpy()
			else {
			// Clamp norm to <1 to avoid atanh domain errors
			val safeNorm = min(xNorm, 1 - 1e-7)
			val factor = tanh(r * atanh(safeNorm)) / xNorm
			x.cpy().scl(factor.toFloat)
			}
		}

		/** Möbius inverse: ⊖u */
		def mobiusInverse(u: Vector3): Vector3 = u.cpy().scl(-1f)
		}

		// Hyperbolic interpolation along geodesic
		object Hyperbolic {

		/**
			* Returns a point along the hyperbolic geodesic from u to v.
			* t=0 -> u, t=1 -> v, t=0.5 -> midpoint
			*/
		def interpolate(u: Vector3, v: Vector3, t: Double = 0.5): Vector3 = {
			val w = Mobius.mobiusAdd(Mobius.mobiusInverse(u), v)
			if (w.len() == 0) u.cpy()
			else {
			val y = Mobius.mobiusScalarMultiply(t, w)
			Mobius.mobiusAdd(u, y)
			}
		}

		def lerp(u:Vector3, v:Vector3, t:Double = 0.5) : Vector3 = {
			u.cpy.lerp(v, t.toFloat)
		}
	}

}