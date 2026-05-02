import Maths3D._
import java.lang.Math._
import com.badlogic.gdx.math.Vector3
import com.badlogic.gdx.math.Vector4
import com.badlogic.gdx.math.Matrix4
import com.badlogic.gdx.math.Matrix4._

import scala.annotation.meta.superArg
import scala.collection.mutable.ArrayBuffer
// import slash.vector._

object Hyperboloid {

    

    //Minkowski Bilinear Form
    def B(u: HVec3, v:HVec3) : Double = {
        u.x*v.x + u.y*v.y + u.z*v.z - u.t*v.t
    }


    def hdist(u:HVec3, v:HVec3) : Double = {
        // println("hdist")
        // println(u.Q)
        // println(v.Q)
        // println(acosh(-B(u,v)))
        acosh(-B(u,v))
    }


    implicit def HVec3(v:Vector4): HVec3 = new HVec3(v.x, v.y, v.z, v.w) 

    class HVec3(_x:Float = 0, _y:Float = 0, _z:Float = 0, _w:Float = 1) extends Vector4(_x,_y,_z,_w){
        //3D Hyperbolic Vector in the Hyperboloid Model

        def t = w

        def hdist = acosh(w)

        def toTuple : (Float,Float,Float,Float) = (x,y,z,t)

        def +(v:HVec3) : HVec3 = {
            super.cpy().add(v)
        }

        def -(v:HVec3) : HVec3 = {
            super.cpy().sub(v)
        }

        def unary_- : HVec3 = {
            new Vector4(-x,-y,-z,w)
        }
        
        def mul(m: Matrix4): HVec3 = {
            val a = m.`val`
            // println("mul")
            // println(Q)
            val v = new Vector4(
                (a(M00) * x + a(M01) * y + a(M02) * z + a(M03) * w),
                (a(M10) * x + a(M11) * y + a(M12) * z + a(M13) * w),
                (a(M20) * x + a(M21) * y + a(M22) * z + a(M23) * w),
                (a(M30) * x + a(M31) * y + a(M32) * z + a(M33) * w)
            )
            // println(v.Q)
            return v
        }

        def Q : Float = x*x + y*y + z*z - t*t

        def to3D = new Vector3(x,y,z).cpy()
        
        def hscl(t : Float) : HVec3 = hlerp(new HVec3, this, t)

        def rotate(axis : Vector3, angle:Float) : HVec3 = {
        //     println("rotate")
        //     println(this)
        //     println(Q)
        //     println(axis)
        //     println(angle)
            val newVec = new Vector4(new Vector3(x,y,z).cpy.rotate(axis, angle), w)
            // println(newVec)
            // println(newVec.Q)
            return newVec
        }

        def rotate(axis : HVec3, angle:Float) : HVec3 = {
            val ax = new Vector3(axis.x,axis.y,axis.z).cpy()
            rotate(ax, angle)
        }

        def toPoincare : Vector3 = {
            val denom = (1 + t)
            val v = new Vector3(x/denom, y/denom, z/denom).cpy
            if (v.x.isInfinite() || v.x.isNaN()){
                v.x = 0
            }
            if (v.y.isInfinite() || v.y.isNaN()){
                v.y = 0
            }
            if (v.z.isInfinite() || v.z.isNaN()){
                v.z = 0
            }
            return v
        }

        def toPoincareArray : ArrayBuffer[Float] = {
            val denom = (1 + t)
            var (vx, vy, vz) = (x/denom, y/denom, z/denom)
            if (vx.isInfinite() || vx.isNaN()){
                vx = 0
            }
            if (vy.isInfinite() || vy.isNaN()){
                vy = 0
            }
            if (vz.isInfinite() || vz.isNaN()){
                vz = 0
            }
            return ArrayBuffer[Float](vx,vy,vz)
        }

        def toFloatArr : ArrayBuffer[Float] = {
            return ArrayBuffer[Float](x,y,z,t)
        }
    } 

    // Creates an HVec3 going in the direction of dir, a (hyperbolic) distance of r from the origin
    def HVec(dir:Vector3, r:Float) : HVec3 = {
        if (r > 0){
            return new Vector4(dir.cpy.setLength(sinh(r).toFloat), cosh(r).toFloat)
        } else if (r < 0){
            return new Vector4(dir.cpy.scl(-1).setLength(sinh(-r).toFloat), cosh(r).toFloat)
        } else {
            return new HVec3   
        }
        
    }


    def hlerp(u:HVec3, v:HVec3, t:Float = 0.5f) : HVec3 = {

        if (u == v){
            return u
        }
        // println("hlerp")
        // println(u)
        // println(v)
        // println(u.Q)
        // println(v.Q)
        val d = hdist(u,v)
        val sinhd = sinh(d).toFloat
        val w = u.cpy.scl((sinh((1-t)*d)/sinhd).toFloat) + v.cpy.scl((sinh(t*d)/sinhd).toFloat)
        // println("w")
        // println(w.Q)
        // println(w)
        
        return w
        
    }


    def TranslationMat(v:HVec3) = {
        val (x,y,z,t) = v.toTuple
        val d = t+1
        new Matrix4(Array(
            x*x/d+1, x*y/d,   x*z/d,   x,
            y*x/d,   y*y/d+1, y*z/d,   y,
            x*z/d,   y*z/d,   z*z/d+1, z,
            x,       y,       z,       t
        ))
    }

    def OuterProduct(u:HVec3, v:HVec3) : Matrix4 = {
        new Matrix4(Array(
            u.x*v.x, u.y*v.x, u.z*v.x, u.w*v.x,
            u.x*v.y, u.y*v.y, u.z*v.y, u.w*v.y,
            u.x*v.z, u.y*v.z, u.z*v.z, u.w*v.z,
            u.x*v.w, u.y*v.w, u.z*v.w, u.w*v.w
        ))
    }

    def IdMatrix : Matrix4 = new Matrix4

    def add(A: Matrix4, B: Matrix4): Matrix4 = {
        new Matrix4(A.`val`.indices.map(i => A.`val`(i) + B.`val`(i)).toArray)
    }

    def scl(A: Matrix4, b: Float): Matrix4 = {
        new Matrix4(A.`val`.map(x => b*x))
    }


    def ReflectionMatrix(p:HVec3, q:HVec3) : Matrix4 = {
        val pmq = p-q
        val u = pmq.cpy.scl((1/sqrt(pmq.Q)).toFloat)
        val M = IdMatrix
        M.`val`(M33) = -1
        
        val outputM = add(IdMatrix, OuterProduct(u,u).mul(scl(M,-2f)))
        // println(outputM)
        return outputM
    }







    /** Normalize a Matrix4 (columns = (x,y,z,t), time-last metric diag(1,1,1,-1)).
     * Detects the timelike column automatically and preserves column ordering.
     */
    def normalizeHMatrix(m: Matrix4): Matrix4 = {
    // helper: minkowski dot for time-last ordering
    def mink(a: Array[Double], b: Array[Double]): Double =
        a(0)*b(0) + a(1)*b(1) + a(2)*b(2) - a(3)*b(3)

    def getCol(i: Int): Array[Double] = Array(
        m.`val`(i*4 + 0).toDouble,
        m.`val`(i*4 + 1).toDouble,
        m.`val`(i*4 + 2).toDouble,
        m.`val`(i*4 + 3).toDouble
    )
    def setCol(out: Matrix4, i: Int, v: Array[Double]): Unit = {
        out.`val`(i*4 + 0) = v(0).toFloat
        out.`val`(i*4 + 1) = v(1).toFloat
        out.`val`(i*4 + 2) = v(2).toFloat
        out.`val`(i*4 + 3) = v(3).toFloat
    }

    def sub(a:Array[Double], b:Array[Double]) = Array(a(0)-b(0), a(1)-b(1), a(2)-b(2), a(3)-b(3))
    def scale(v:Array[Double], s:Double) = Array(v(0)*s, v(1)*s, v(2)*s, v(3)*s)

    // project u onto v (Minkowski)
    def project(u:Array[Double], v:Array[Double]): Array[Double] = {
        val denom = mink(v,v)
        if (math.abs(denom) < 1e-15) Array(0.0,0.0,0.0,0.0)
        else scale(v, mink(u,v) / denom)
    }

    def normalizeTimelike(v:Array[Double]): Array[Double] = {
        val q = mink(v,v)
        if (q >= -1e-15) {
        // fallback: ensure a future-pointing time-like vector
        val pert = Array(v(0), v(1), v(2), if (v(3)==0) 1.0 else math.signum(v(3)))
        val qq = mink(pert, pert)
        if (qq >= 0.0) Array(0.0,0.0,0.0,1.0) // fallback to time-axis
        else {
            val s = 1.0/math.sqrt(-qq)
            val r = scale(pert, s)
            if (r(3) < 0.0) scale(r, -1.0) else r
        }
        } else {
        val s = 1.0/math.sqrt(-q)
        val r = scale(v, s)
        if (r(3) < 0.0) scale(r, -1.0) else r
        }
    }

    def normalizeSpacelike(v:Array[Double]): Array[Double] = {
        val q = mink(v,v)
        if (q <= 1e-15) {
        // fallback orthonormal vector
        Array(1.0,0.0,0.0,0.0)
        } else {
        scale(v, 1.0/math.sqrt(q))
        }
    }

    // read all columns
    val cols = Array.tabulate(4)(getCol)

    // find the timelike column index (most negative mink dot), if none pick last
    val norms = cols.map(c => mink(c,c))
    val (tIdx, tVal) = norms.zipWithIndex.minBy(_._1) // min by norm (most negative)
    val timelikeIdx =  3 // if none <0, use column 3

    // Build orthonormal basis in the order: [timelike, others in original order excluding timelike]
    val order = (0 until 4).filter(_ != timelikeIdx).toArray
    val basisIdx = Array(timelikeIdx) ++ order

    // container for orthonormal results
    val orth = Array.ofDim[Double](4,4)

    // e0 = normalized timelike
    orth(timelikeIdx) = normalizeTimelike(cols(timelikeIdx))

    // for the remaining columns, Gram-Schmidt against previous orth vectors (Minkowski)
    for (j <- order.indices) {
        val idx = order(j)
        // start with original column
        var v = cols(idx)
        // subtract projections onto already computed orth vectors
        // note: project uses minkowski metric with sign handling
        v = sub(v, project(v, orth(timelikeIdx)))
        for (k <- 1 to j) {
        val prevIdx = order(k-1)
        v = sub(v, project(v, orth(prevIdx)))
        }
        // normalize as spacelike
        orth(idx) = normalizeSpacelike(v)
    }

    // assemble output matrix with orthonormal columns placed into their original indices
    val out = new Matrix4()
    for (i <- 0 until 4) setCol(out, i, orth(i))

    // ensure orientation positive (flip last column if needed)
    def det4(A: Matrix4): Double = {
        val v = A.`val`
        def e(r:Int,c:Int) = v(c*4 + r).toDouble
        val m00=e(0,0); val m01=e(0,1); val m02=e(0,2); val m03=e(0,3)
        val m10=e(1,0); val m11=e(1,1); val m12=e(1,2); val m13=e(1,3)
        val m20=e(2,0); val m21=e(2,1); val m22=e(2,2); val m23=e(2,3)
        val m30=e(3,0); val m31=e(3,1); val m32=e(3,2); val m33=e(3,3)
        m00 * (m11*(m22*m33 - m23*m32) - m12*(m21*m33 - m23*m31) + m13*(m21*m32 - m22*m31)) -
        m01 * (m10*(m22*m33 - m23*m32) - m12*(m20*m33 - m23*m30) + m13*(m20*m32 - m22*m30)) +
        m02 * (m10*(m21*m33 - m23*m31) - m11*(m20*m33 - m23*m30) + m13*(m20*m31 - m21*m30)) -
        m03 * (m10*(m21*m32 - m22*m31) - m11*(m20*m32 - m22*m30) + m12*(m20*m31 - m21*m30))
    }

    if (det4(out) < 0.0) {
        // flip the last orth column in-place (choose column 3 for flip, fallback if that's timelike)
        val flipIdx = if (timelikeIdx != 3) 3 else order(0)
        val old = Array(out.`val`(flipIdx*4+0).toDouble, out.`val`(flipIdx*4+1).toDouble, out.`val`(flipIdx*4+2).toDouble, out.`val`(flipIdx*4+3).toDouble)
        setCol(out, flipIdx, scale(old, -1.0))
    }

    out
    }


}