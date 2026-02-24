import Maths3D._
import java.lang.Math._
import com.badlogic.gdx.math.Vector3
import com.badlogic.gdx.math.Vector4
import com.badlogic.gdx.math.Matrix4
import com.badlogic.gdx.math.Matrix4._

import scala.annotation.meta.superArg
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
    } 

    // Creates an HVec3 going in the direction of dir, a (hyperbolic) distance of r from the origin
    def HVec(dir:Vector3, r:Float) : HVec3 = {
        new Vector4(dir.setLength(sinh(r).toFloat), cosh(r).toFloat)
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






}