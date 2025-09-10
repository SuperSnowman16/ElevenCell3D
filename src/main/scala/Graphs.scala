import com.badlogic.gdx.math.Vector3
import scala.collection.mutable.ListBuffer
import java.lang.Math._
import MyGame.{stickerSize, depth}
import Maths3D._
import Maths3D.Hyperbolic.lerp
import Maths3D.Hyperbolic.interpolate
import Maths3D.Mobius.mobiusScalarMultiply
import scala.collection.mutable.HashMap
object Graphs {

		val (p,q) = (3,5)


		def tanharcosh(x:Double) : Float = (sqrt(x-1)/sqrt(x+1)).toFloat

		def radii353 : (Float,Float,Float) = {
			val f = tanharcosh((sqrt(15)+sqrt(3))/4)
			val e = tanharcosh((3+sqrt(5))/(2*sqrt(3)))
			val r3 = atanh(sqrt(16*sqrt(5) - 35))
			val v = tanh(r3/2).toFloat

			(f,e,v)
		}

		def firstVectors : (Vector3, Vector3, Vector3) = {
				val (f,e,v) = radii353
				val r1 = new Vector3(f, 0, 0)

				val sinA = sqrt((3-sqrt(5))/6).toFloat
				val cosA = sqrt((3+sqrt(5))/6).toFloat

				val r2 = new Vector3(e*cosA, e*sinA, 0)

				val sinB = sqrt(2*(5 - sqrt(5))/15).toFloat
				val cosB = sqrt((5 + 2*sqrt(5))/15).toFloat

				val r3 = new Vector3(v*cosB,v*sinB,0).rotate(new Vector3(1,0,0), 60)


				(r1,r2,r3)
				
		}

		

		def orbit(axis:Vector3, vec:Vector3, n:Int) : (Array[Vector3]) = {
			((1 until n).map(x => new Vector3(vec).rotate(axis, -x*360f/n)).prepended(vec)).toArray
		}

		class Graph(val faces:Array[Face], val edges:Array[Edge], val verts:Array[Vertex], var midpoint:Vector3, val id:Int, val color:Int, val isMirrored:Boolean){

			def mirror(faceID:Int) : Graph = {
				val newFaces = new Array[Face](faces.size)
				val newEdges = new Array[Edge](edges.size)
				val newVerts = new Array[Vertex](verts.size)

				val f0 = faces(faceID)
				val vs = f0.verts.map(v => v.pt)
				val sphere = circumsphere(f0.pt, vs(0), vs(1), vs(2)).get

				for (i <- 0 until faces.length){
					val f = faces(i)
					val newF = new Face(f.id, f.p, f.oppCell, sphereMirror(f.pt, sphere))
					newFaces(i) = newF
				}
				for (i <- 0 until edges.length){
					val e = edges(i)
					val newE = new Edge(e.id, sphere.inv(e.pt))
					newEdges(i) = newE
				}
				for (i <- 0 until verts.length){
					val v = verts(i)
					val newV = new Vertex(v.id,v.q, sphere.inv(v.pt))
					newVerts(i) = newV
				}

				newFaces(faceID).oppCell = color
				newFaces(getOpp(faceID)).oppCell = color





				for (i <- 0 until faces.length){
					val f = faces(i)
					val newF = newFaces(i)
					for (j <- 0 until f.p){
						newF.edges(j) = newEdges(f.edges(j).id)
						newF.verts(j) = newVerts(f.verts(j).id)
					}
				}

				for (i <- 0 until edges.length){
					val e = edges(i)
					val newE = newEdges(i)
					for (j <- 0 until e.size){
						newE.faces(j) = newFaces(e.faces(j).id)
					}
				}

				for (i <- 0 until verts.length){
					val v = verts(i)
					val newV = newVerts(i)
					for (j <- 0 until v.q){
						newV.faces(j) = newFaces(v.faces(j).id)
					}
				}

				val newF0 = newFaces(faceID)
				for (v <- newFaces(faceID).verts){
					val f1 = v.getOffsetFace(newF0, 2)
					val f2 = v.getOffsetFace(newF0, -2)
					val temp = f1.oppCell
					f1.oppCell = f2.oppCell
					f2.oppCell = temp 
					newFaces(getOpp(f1.id)).oppCell = f1.oppCell
					newFaces(getOpp(f2.id)).oppCell = f2.oppCell

				}

				for (i <- 0 until faces.length){
					val f = faces(i)
					val newF = newFaces(i)
					val mirr = sphereMirror(_, sphere)
					newF.edgePts = f.edgePts.map(x => x.map(mirr))
					newF.vertPts = f.vertPts.map(x => x.map(mirr))
					newF.ridgePts = f.ridgePts.map(mirr)
					newF.centerPts = f.centerPts.map(mirr)
					newF.centerMidpt = mirr(f.centerMidpt)

				}



				new Graph(newFaces, newEdges, newVerts, sphericalInversion(midpoint, sphere).get, faceID, f0.oppCell, !isMirrored)
			}

			def getOpp(i:Int) : Int = (i+10)%20


			def transform(tf: Vector3 => Vector3) {
				faces.foreach(n => n.pt = tf(n.pt))
				edges.foreach(n => n.pt = tf(n.pt))
				verts.foreach(n => n.pt = tf(n.pt))
				faces.map(n => n.centerMidpt = tf(n.centerMidpt))
				faces.map(_.ridgePts.map(tf))
				faces.map(_.edgePts.map(_.map(tf)))
				faces.map(_.vertPts.map(_.map(tf)))
				faces.map(_.centerPts.map((tf)))



				midpoint = tf(midpoint)

			}



			
		}

		


		def GenerateGraph : Graph = {


			val (f1, e1, v1) = firstVectors
			val es = orbit(f1, e1, 3)
			val vs = orbit(f1, v1, 3)
			val faceArr : Array[Face] = new Array(20)
			val edgeList : ListBuffer[Edge] = new ListBuffer()
			val vertList : ListBuffer[Vertex] = new ListBuffer()

			val face0 = new Face(0, p, 0)
			face0.pt = f1
			faceArr(0) = face0
			for (i <- 1 until 20){
				faceArr(i) = new Face(i, p, i%10)
			}
			

			for (i <- 0 until 20){
				for (j <- 0 until 3){
					faceArr(i).faces(j) = faceArr(icosaNbrs(i)(j))
				}
			}

			for (i <- 0 until 3){
				val newV = new Vertex(i, q)
				newV.pt = vs(i)
				newV.setFace(0, face0)
				face0.verts(i) = newV
				connectVertex(newV, face0)
				vertList += newV

				val newE = new Edge(i)
				newE.pt = es(i)
				newE.setFace(0, face0)
				face0.setEdge(i, newE)
				edgeList += newE
				val oppFace = faceArr(icosaNbrs(0)(i))
				newE.setFace(1, oppFace)
				oppFace.setEdge(0, newE)
				oppFace.setFace(0, face0)
				oppFace.pt = new Vector3(face0.pt).rotate(newE.pt, 180)
				
			}

			for (i <- 0 until 20){
				for (j <- 0 until 3){
					if (faceArr(i).getVertex(j) == null){
						val fi = faceArr(i)
						val newV = new Vertex(i, q)
						vertList += newV						
						newV.setFace(0, fi)
						fi.verts(j) = newV
						connectVertex(newV, fi)
					}
					
				}
			}

			var done = false
			while (!done){
				done = true
				for (i <- 1 until 20){
					val face = faceArr(i)
					val vScore = face.vertexScore._1
					if (vScore < p){
						done = false
						if (vScore > 0)
						orbitVertex(face)
					}
					// println("face")

					val eScore = face.edgeScore._1
					if (eScore < p){
						done = false
						if (eScore > 0)
						orbitEdge(face, edgeList)
					}
					// println("edge")


					val fScore = face.faceScore._1
					if (fScore < p){
						done = false
						if (fScore > 0)
						orbitFace(face)
					}
					// println("vertex")
				}
				println("loop")

			}
			println("done")

			val edgeArr = edgeList.toArray
			for (i <- 0 until edgeArr.length){
				edgeArr(i).id = i
			}
			val vertArr = vertList.toArray
			for (i <- 0 until vertArr.length){
				vertArr(i).id = i
			}

			faceArr.foreach(_.generateStickers)


				
			new Graph(faceArr, edgeArr, vertArr, new Vector3, 20, 10, false)
		}

		def connectVertex(vertex:Vertex, startFace:Face){
			var vId = vertex.findFace(startFace)
			var fId = startFace.findVertex(vertex)
			var prevFace = startFace
			while (vertex.faces.map(_ == null).reduce(_||_)){
				vId += 1
				val nextFace = prevFace.getFace(fId-1)
				fId = nextFace.findFace(prevFace)
				vertex.setFace(vId, nextFace)
				nextFace.setVertex(fId, vertex)
				prevFace = nextFace
			}
		}

		def orbitVertex(startFace:Face){
			val firstV = startFace.vertexScore._2
			val pt0 = startFace.getVertex(firstV).pt
			var offset = 1
			while (startFace.vertexScore._1 < p){
				val nextV = startFace.getVertex(firstV + offset)
				if (nextV.pt == null){
					nextV.pt = new Vector3(pt0).rotate(startFace.pt, -offset*120)
				}
				offset += 1
			}
		}

		def orbitEdge(startFace:Face, edgeList:ListBuffer[Edge]){
			val firstE = startFace.edgeScore._2
			val pt0 = startFace.getEdge(firstE).pt
			var offset = 1
			while (startFace.edgeScore._1 < p){
				val id = firstE + offset
				val nextE = startFace.getEdge(id)
				if (nextE == null){
					val newE = new Edge
					newE.setFace(0, startFace)
					newE.setFace(1, startFace.getFace(id))
					edgeList += newE
					newE.pt = new Vector3(pt0).rotate(startFace.pt, -offset*120)
					startFace.setEdge(id, newE)
					val nbFace = startFace.getFace(id)
					nbFace.setEdge(nbFace.findFace(startFace), newE)
				}
				offset += 1
			}
		}

		def orbitFace(startFace:Face){
			val firstF = startFace.faceScore._2
			val pt0 = startFace.getFace(firstF).pt
			var offset = 1
			while (startFace.faceScore._1 < p){
				val nextF = startFace.getFace(firstF + offset)
				if (nextF.pt == null){
					nextF.pt = new Vector3(pt0).rotate(startFace.pt, -offset*120)
				}
				offset += 1
			}
		}


		

		class Node(var pt:Vector3, var id:Int, val size:Int){

			val faces = new Array[Face](size)

			
			def setFace(id:Int, nb:Face) : Boolean = {
				val index = mod(id, size)
				if (faces(index) == null){
						faces(index) = nb
						return true
				}
				return false
			}

			def getFace(index:Int) : Face = {
					faces(mod(index, size))
			}

			

			def findFace(f:Face) : Int = {
				for (i <- 0 until size){
						if (faces(i) == f){
						return i
						} 
				}
				println("findFace error ")//+ name +" to " + f.name)
				// throw new Exception("findFace error")
				return -1
			}

			def getOffsetFace(f:Face, offset:Int) : Face = {
				getFace(findFace(f)+offset)
			}

			def faceScore : (Int, Int) = {
				if (pt == null){
					return (0, -1)
				}
				var count = 0
				var firstScore = -1
				for (i <- 0 until size){
					if (faces(i).pt != null){
						count += 1
						if (firstScore == -1){
							firstScore = i
						}
					}
				}
				return (count, firstScore)
			}


			
		}
			
		

		class Face(id:Int, val p:Int, var oppCell:Int, vec:Vector3 = null) extends Node(vec, id, p){

			// val thisFace = this

			val verts = new Array[Vertex](size)
			val edges = new Array[Edge](size)
			
			var centerMidpt = new Vector3
			var centerPts = new Array[Vector3](size*2)
			var ridgePts = new Array[Vector3](size*2)
			var edgePts = Array.ofDim[Vector3](size,6)
			var vertPts = Array.ofDim[Vector3](size,4)

			def generateStickers {
				val ridgeMidpts = new Array[Vector3](size)
				val edgeMidpts = Array.ofDim[Vector3](size,2)

				for (i <- 0 until size){
					ridgeMidpts(i) = interpolate(verts(i).pt, pt, depth*.75f)
					ridgePts(2*i) = interpolate(verts(i).pt, ridgeMidpts(i), 1/stickerSize)
					edgeMidpts(i)(0) = interpolate(verts(i).pt, edges(i).pt, depth*.5)
					edgeMidpts(i)(1) = interpolate(getVertex(i+1).pt, edges(i).pt, depth*.5)
				}

				for (i <- 0 until size){
					ridgePts(2*i+1) = interpolate(ridgePts(2*i), ridgePts(mod(2*i+2, 2*size)))
					val (e,v) = (edges(i).pt, verts(i).pt)
					vertPts(i)(0) = v
					vertPts(i)(1) = interpolate(v, edgeMidpts(i)(0), stickerSize)
					vertPts(i)(2) = interpolate(v, ridgeMidpts(i), stickerSize)
					vertPts(i)(3) = interpolate(v, edgeMidpts(mod(i-1,size))(1), stickerSize)
				}

				for (i <- 0 until size){
					val (e,v, v2) = (edges(i).pt, verts(i).pt, verts((i+1)%size).pt)

					edgePts(i)(0) = e
					edgePts(i)(1) = interpolate(v2, edgeMidpts(i)(1), 1/stickerSize)
					edgePts(i)(2) = interpolate(vertPts((i+1)%size)(1), vertPts((i+1)%size)(2), 1/(stickerSize*stickerSize))
					edgePts(i)(4) = interpolate(vertPts(i)(3), vertPts(i)(2), 1/(stickerSize*stickerSize))
					edgePts(i)(5) = interpolate(v, edgeMidpts(i)(0), 1/stickerSize)

					edgePts(i)(3) = interpolate(edgePts(i)(2), edgePts(i)(4))

				}

				val (midpt, pts) = getPolygon
				val f = mobiusScalarMultiply(.9f, _)
				centerMidpt = f(midpt)
				centerPts = pts.map(f)



			}

			def FaceTwistFn(dir:Int) : (Int => Int) = {

				val map = new HashMap[Int,Int]
				for (i <- 0 until 3){
					for (j <- 1 to 3){
						map.addOne(getVertex(i).getOffsetFace(this, j).oppCell, getVertex(i+dir).getOffsetFace(this, j).oppCell)
					}
				}
				return x => map.get(x) match {
					case None => x
					case Some(value) => value
				}
				
			}
			
			



			

			def setVertex(id:Int, nb:Vertex) : Boolean = {
				val index = mod(id, size)
				if (verts(index) == null){
						verts(index) = nb
						return true
				}
				return false
			}

			def getVertex(index:Int) : Vertex = {
				verts(mod(index, size))
			}

			def findVertex(v:Vertex) : Int = {
				for (i <- 0 until size){
						if (verts(i) == v){
						// println(i)
						return i
						} 
				}
				println("findVertex error ")//+ name +" to " + v.name)
				// throw new Exception("findVertex error")
				return -1
			}



			

			def vertexScore : (Int, Int) = {
				if (pt == null){
					return (0, -1)
				}
				var count = 0
				var firstScore = -1
				for (i <- 0 until size){
					if (verts(i).pt != null){
						count += 1
						if (firstScore == -1){
							firstScore = i
						}
					}
				}
				return (count, firstScore)
			}

			def setEdge(id:Int, nb:Edge) : Boolean = {
				val index = mod(id, size)
				if (edges(index) == null){
						edges(index) = nb
						return true
				}
				return false
			}

			def getEdge(index:Int) : Edge = {
				edges(mod(index, size))
			}

			def findEdge(v:Vertex) : Int = {
				for (i <- 0 until size){
						if (edges(i) == v){
						// println(i)
						return i
						} 
				}
				println("findEdge error ")//+ name +" to " + v.name)
				// throw new Exception("findVertex error")
				return -1
			}

			def edgeScore : (Int, Int) = {
				if (pt == null){
					return (0, -1)
				}
				var count = 0
				var firstScore = -1
				for (i <- 0 until size){
					if (edges(i) != null){
						count += 1
						if (firstScore == -1){
							firstScore = i
						}
					}
				}
				return (count, firstScore)
			}


			def getPolygon : (Vector3, Array[Vector3]) = {
				val arr = new Array[Vector3](p*2)
				for (i <- 0 until p){
					arr(2*i) = verts(i).pt
					arr(2*i+1) = edges(i).pt
					// arr(2*i+1) = interpolate(arr(2*i), arr(2*i+2))
					// arr(2*i+3) = interpolate(arr(2*i+2), arr((2*i+2)%(p*4)))
				}

				return (pt, arr)
			}


		}

			

		class Edge(id:Int = -1, vec:Vector3 = null) extends Node(vec, id, 2){
			
		}

		class Vertex(id:Int = -1, val q:Int, vec:Vector3 = null) extends Node(vec, id, q){

		}


		val hemiIcosaNbrs = Array(
			Array(1,4,7),
			Array(0,9,2),
			Array(1,16,3),
			Array(2,18,4),
			Array(0,3,5),
			Array(4,19,6),
			Array(5,12,7),
			Array(0,6,8),
			Array(7,13,9),
			Array(1,8,15)
		)

		val icosaNbrs = hemiIcosaNbrs ++ hemiIcosaNbrs.map(_.map(x => (x+10)%20).reverse)
		println(icosaNbrs.map(_.mkString(",")).mkString("\n"))

}