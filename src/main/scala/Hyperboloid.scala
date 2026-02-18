import Maths3D._
import java.lang.Math._
import com.badlogic.gdx.math.Vector3
import com.badlogic.gdx.math.Vector4
import com.badlogic.gdx.math.Matrix4
import scala.annotation.meta.superArg
// import slash.vector._

object Hyperboloid {

    
    

   

    //Minkowski Bilinear Form
    def B(u: HVec3, v:HVec3) : Double = {
        u.x*v.x + u.y*v.y + u.z*v.z - u.t*v.t
    }


    def hdist(u:HVec3, v:HVec3) : Double = acosh(-B(u,v))
    


    implicit def HVec3(v:Vector4): HVec3 = new HVec3(v.x, v.y, v.z, v.w) 

    class HVec3(x:Float = 0, y:Float = 0, z:Float = 0, w:Float = 1) extends Vector4(x,y,z,w){
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

        def Q : Float = x*x + y*y + z*z - t*t

        
        def hscl(t : Float) : HVec3 = hlerp(new HVec3, this, t)


        def toPoincare : Vector3 = {
            val denom = (1 + t)
            return new Vector3(x/denom, y/denom, z/denom)
        }
    } 


    def hlerp(u:HVec3, v:HVec3, t:Float) : HVec3 = {
        val d = hdist(u,v)
        val sinhd = sinh(d).toFloat
        return u.scl((sinh((1-t)*d)/sinhd).toFloat) + v.scl((sinh(t*d)/sinhd).toFloat)
    }


    def TranslationMat(v:HVec3) = {
        val (x,y,z,t) = v.toTuple
        val d = t+1
        new Matrix4(Array(
            x*x/d+1, x*y/d,   x*z/d,   x,
            y*x/d,   y*y/d+1, y*z/d,   y,
            x*z/d+1, x*y/d,   x*z/d+1, x,
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

    def add(a: Matrix4, b: Matrix4): Matrix4 = {
        new Matrix4(a.`val`.indices.map(i => a.`val`(i) + b.`val`(i)).toArray)
    }


    def ReflectionMatrix(p:HVec3, q:HVec3) : Matrix4 = {
        val pmq = p-q
        val u = pmq.scl(sqrt(pmq.Q).toFloat)
        val M = IdMatrix
        M.`val`(Matrix4.M33) = -1
        return add(IdMatrix, OuterProduct(u,u).mul(M).scl(-2))
    }






}