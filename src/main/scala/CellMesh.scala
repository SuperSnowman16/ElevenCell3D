import Graphs.Graph
import Hyperboloid.HVec3
import com.badlogic.gdx.graphics.Color
import scala.collection.mutable.ArrayBuffer
import com.badlogic.gdx.graphics.Mesh
import com.badlogic.gdx.graphics.VertexAttribute
import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.math.Matrix4
import com.badlogic.gdx.math.Vector3

object CellMeshes {

    val redArr = ArrayBuffer(1f, 0f, 0f, 1f)
    val greenArr = ArrayBuffer(0f, 1f, 0f, 1f)
    val blueArr = ArrayBuffer(0f, 0f, 1f, 1f)


    class CellMesh(g:Graph, main:Main) {
        val triangleArr = new ArrayBuffer[Triangle]
        val lineArr = new ArrayBuffer[Line]
        val state = main.state
        val colors = main.colors
        val cell = g.color
        

        
        for (f <- g.faces){
            var p = TriangularisePolygon2(f.centerMidpt, f.centerPts, main.colors(cell), "c"+g.id+"m"+f.id)
            triangleArr.addAll(p._1)
            lineArr.addAll(p._2)

            val ridgeColor = colors(state.get(cell, Set(cell, f.oppCell)))
            p = TriangularisePolygon2(f.stickerPt, f.ridgePts, ridgeColor, "c"+g.id+"f"+f.id)
            triangleArr.addAll(p._1)
            lineArr.addAll(p._2)

            for (i <- 0 until f.p){
                val edgeColor = colors(state.get(cell, f.edges(i).faces.map(_.oppCell).appended(cell).toSet))
                p = TriangularisePolygon(f.edgePts(i), edgeColor, "c"+g.id+"e"+f.edges(i).id)
                triangleArr.addAll(p._1)
                lineArr.addAll(p._2)

                val vertColor = colors(state.get(cell, f.verts(i).faces.map(_.oppCell).appended(cell).toSet))
                p = TriangularisePolygon(f.vertPts(i), vertColor, "c"+g.id+"v"+f.verts(i).id)
                triangleArr.addAll(p._1)
                lineArr.addAll(p._2)

                
                val petalColor = colors(state.get(cell, (-1 to 1).map(j => f.verts(i).getOffsetFace(f, j).oppCell).appended(cell).toSet))
                p = TriangularisePolygon(f.petalPts(i), petalColor, "c"+g.id+"p"+i+"f"+f.id)
                triangleArr.addAll(p._1)
                lineArr.addAll(p._2)

                val wingColor = colors(state.get(cell, (-1 to 2).map(j => f.verts(i).getOffsetFace(f, j).oppCell).appended(cell).toSet))
                p = TriangularisePolygon(f.wingPts(2*i), wingColor, "c"+g.id+"w"+(2*i)+"f"+f.id)
                // println(p)

                triangleArr.addAll(p._1)
                lineArr.addAll(p._2)

                val wingColor2 = colors(state.get(cell, (-2 to 1).map(j => f.verts(i).getOffsetFace(f, j).oppCell).appended(cell).toSet))
                p = TriangularisePolygon(f.wingPts(2*i+1), wingColor2, "c"+g.id+"w"+(2*i+1)+"f"+f.id)
                // println(p)

                triangleArr.addAll(p._1)
                lineArr.addAll(p._2)

            }
        }

        val triangleVertexData = triangleArr.flatMap(_.toFloatArr).toArray
        val triangleMesh = new Mesh(false, triangleArr.length*3, 0,
            new VertexAttribute(Usage.Position, 4, "a_hyper"),
            new VertexAttribute(Usage.ColorUnpacked, 4, "a_color")
        )

        triangleMesh.setVertices(triangleVertexData)

        val lineVertexData = lineArr.flatMap(_.toFloatArr).toArray
        val lineMesh = new Mesh(false, lineArr.length*2, 0,
            new VertexAttribute(Usage.Position, 4, "a_hyper"),
            new VertexAttribute(Usage.ColorUnpacked, 4, "a_color")
        )

        lineMesh.setVertices(lineVertexData)


        def updateMesh(): Unit = {
            var idx = 0
            for (triangle <- triangleArr) {
                triangle.updateColor
                val c = triangle.color
                for (_ <- 0 until 3) {
                    triangleVertexData(idx + 4) = c.r
                    triangleVertexData(idx + 5) = c.g
                    triangleVertexData(idx + 6) = c.b
                    triangleVertexData(idx + 7) = c.a
                    idx += 8
                }
            }
            triangleMesh.updateVertices(0, triangleVertexData, 0, triangleVertexData.length)
        }

        def midpoint(transform : Matrix4) : HVec3 = {
            g.midpoint.mul(transform)
        }

        def midpointDist(transform: Matrix4) : Double = {
            g.midpoint.mul(transform).hdist
        }


        class Triangle(val v1:HVec3, val v2:HVec3, val v3:HVec3, var color:Color, val name:String){
            def toFloatArr : ArrayBuffer[Float] = {
                val colArr = ArrayBuffer(color.r, color.g, color.b, color.a)
                if (color.a == 0f){
                    println("zero")
                }
                v1.toFloatArr ++ colArr ++ v2.toFloatArr ++ colArr  ++ v3.toFloatArr ++ colArr
            }

            def toFloatArr(transform: Matrix4) : ArrayBuffer[Float] = {
                val colArr = ArrayBuffer(color.r, color.g, color.b, color.a)
                v1.mul(transform).toFloatArr ++ colArr ++ v2.mul(transform).toFloatArr ++ colArr  ++ v3.mul(transform).toFloatArr ++ colArr
                // v1.mul(transform).toPoincareArray ++ redArr ++ v2.mul(transform).toPoincareArray ++ blueArr  ++ v3.mul(transform).toPoincareArray ++ greenArr

            }

            def updateColor {
                color = colors(state.get(main.meshIDtoGrip(name)))
               
            }

            def toVec3(transform : Matrix4): (Vector3,Vector3,Vector3) = {
                (v1.mul(transform).toPoincare, v2.mul(transform).toPoincare, v3.mul(transform).toPoincare)
            }
        }

        class Line(val v1:HVec3, val v2:HVec3){
            def toFloatArr : ArrayBuffer[Float] = {
                val colArr = ArrayBuffer(0f, 0f, 0f, 1f)
                v1.toFloatArr ++ colArr ++ v2.toFloatArr ++ colArr
            }
            def toFloatArr(transform: Matrix4) : ArrayBuffer[Float] = {
                val colArr = ArrayBuffer(0f ,0f , 0f, 1f)
                v1.mul(transform).toFloatArr ++ colArr ++ v2.mul(transform).toFloatArr ++ colArr 
            }
        }


        def TriangularisePolygon(pts:Array[HVec3], color:Color, name:String) : (ArrayBuffer[Triangle],ArrayBuffer[Line]) = {
            val triangles = new ArrayBuffer[Triangle]
            val lines = new ArrayBuffer[Line]
            val p = pts(0)
            lines.addOne(new Line(p, pts(1)))
            lines.addOne(new Line(p, pts.last))
            

            for(i <- 1 until pts.size-1){
                triangles.addOne(new Triangle(p, pts(i), pts(i+1), color, name))
                lines.addOne(new Line(pts(i), pts(i+1)))
            }



            return (triangles, lines)
        }

        def TriangularisePolygon2(midp:HVec3, pts:Array[HVec3], color:Color, name:String) : (ArrayBuffer[Triangle],ArrayBuffer[Line]) = { 
            val triangles = new ArrayBuffer[Triangle]
            val lines = new ArrayBuffer[Line]


            for(i <- 0 until pts.size-1){
                triangles.addOne(new Triangle(midp, pts(i), pts(i+1), color, name))
                lines.addOne(new Line(pts(i), pts(i+1)))
            }
            triangles.addOne(new Triangle(midp, pts.last, pts(0), color, name))
            lines.addOne(new Line(pts.last, pts(0)))

            return (triangles, lines)


        }

    }

    



    

}




