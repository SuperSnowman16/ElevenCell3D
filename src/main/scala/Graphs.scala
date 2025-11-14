import com.badlogic.gdx.math.Vector3
import scala.collection.mutable.ListBuffer
import java.lang.Math._
import Main.{stickerSize, cutDepth}
import Maths3D._
import Maths3D.Mobius.mobiusScalarMultiply
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

import Main.cellSize
import Main.centerSize
import Permutations.Permutation
object Graphs {

		val (p,q) = (5,3)

		
		def coshToReal(x:Double) : Float = (sqrt(x-1)/sqrt(x+1)).toFloat

		def radii535 : (Float,Float,Float) = {
			val cosB = sqrt((5 + 2*sqrt(5))/15)
			val r1 = acosh(sqrt(5+2*sqrt(5))/2)
			val f = coshToReal(sqrt(5+2*sqrt(5))/2)
			val e = coshToReal(sqrt(5/2 + 11/(2*sqrt(5))))
			// val r3 = atanh(3*sqrt((8*sqrt(5)-15)*(19-4*sqrt(15)))/11)
			val r3 = atanh(tanh(r1)/cosB)
			val v = tanh(r3/2).toFloat

			(f,e,v)
		}

		def firstVectors : (Vector3, Vector3, Vector3) = {
				val (f,e,v) = radii535
				val r1 = new Vector3(f, 0, 0)

				val sinA = sqrt((5 - sqrt(5))/10).toFloat
				val cosA = sqrt((5 + sqrt(5))/10).toFloat

				// println("trig" + (sinA*sinA+cosA*cosA))

				val r2 = new Vector3(e*cosA, e*sinA, 0)

				val sinB = sqrt(2*(5 - sqrt(5))/15).toFloat
				val cosB = sqrt((5 + 2*sqrt(5))/15).toFloat

				val r3 = new Vector3(v*cosB,v*sinB,0).rotate(new Vector3(1,0,0), 36)


				(r1,r2,r3)
				
		}

		

		def orbit(axis:Vector3, vec:Vector3, n:Int) : (Array[Vector3]) = {
			((1 until n).map(x => new Vector3(vec).rotate(axis, -x*360f/n)).prepended(vec)).toArray
		}

		class Graph(val faces:Array[Face], val edges:Array[Edge], val verts:Array[Vertex], var midpoint:Vector3, var id:Int, val color:Int, val isMirrored:Boolean){

			def mirror(faceID:Int) : Graph = {
				val newFaces = new Array[Face](faces.size)
				val newEdges = new Array[Edge](edges.size)
				val newVerts = new Array[Vertex](verts.size)

				val f0 = faces(faceID)
				val vs = f0.verts.map(v => v.pt)
				val mirrorPerm = f0.faceMirror
				val sphere = circumsphere(f0.pt, vs(0), vs(1), vs(2)).get

				for (i <- 0 until faces.length){
					val f = faces(i)
					val newF = new Face(f.id, f.p, mirrorPerm(f.oppCell), sphereMirror(f.pt, sphere))
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
						newF.faces(j) = newFaces(f.faces(j).id)
						newF.edges(j) = newEdges(f.edges(j).id)
						newF.verts(j) = newVerts(f.verts(j).id)
					}
					newF.twistPerm = mirrorPerm * f.twistPerm * mirrorPerm
				}

				for (i <- 0 until edges.length){
					val e = edges(i)
					val newE = newEdges(i)
					for (j <- 0 until e.size){
						newE.faces(j) = newFaces(e.faces(j).id)
					}
					newE.twistPerm = mirrorPerm * e.twistPerm * mirrorPerm
				}

				for (i <- 0 until verts.length){
					val v = verts(i)
					val newV = newVerts(i)
					for (j <- 0 until v.q){
						newV.faces(j) = newFaces(v.faces(j).id)
					}
					newV.twistPerm = mirrorPerm * v.twistPerm * mirrorPerm
				}

				val newF0 = newFaces(faceID)
				// for (v <- newFaces(faceID).verts){
				// 	val f1 = v.getOffsetFace(newF0, 2)
				// 	val f2 = v.getOffsetFace(newF0, -2)
				// 	// val temp = f1.oppCell
				// 	// f1.oppCell = f2.oppCell
				// 	// f2.oppCell = temp 
				// 	newFaces(getOpp(f1.id)).oppCell = f1.oppCell
				// 	newFaces(getOpp(f2.id)).oppCell = f2.oppCell

				// }

				for (i <- 0 until faces.length){
					val f = faces(i)
					val newF = newFaces(i)
					val mirr = sphereMirror(_, sphere)
					newF.edgePts = f.edgePts.map(x => x.map(mirr))
					newF.vertPts = f.vertPts.map(x => x.map(mirr))
					newF.ridgePts = f.ridgePts.map(mirr)
					newF.centerPts = f.centerPts.map(mirr)
					newF.centerMidpt = mirr(f.centerMidpt)

					newF.faceMirror = f0.faceMirror * f.faceMirror * f0.faceMirror

				}



				new Graph(newFaces, newEdges, newVerts, sphericalInversion(midpoint, sphere).get, faceID, f0.faceMirror(color), !isMirrored)
			}

			def getOpp(i:Int) : Int = (i+6)%12


			def transform(tf: Vector3 => Vector3) {
				for (face <- faces){
					face.pt = tf(face.pt)
					face.ridgePts = face.ridgePts.map(tf)
					face.edgePts = face.edgePts.map(_.map(tf))
					face.vertPts = face.vertPts.map(_.map(tf))
					face.centerPts = face.centerPts.map((tf))
					face.centerMidpt = tf(face.centerMidpt)
				}
				
				edges.foreach(n => n.pt = tf(n.pt))
				verts.foreach(n => n.pt = tf(n.pt))
				midpoint = tf(midpoint)

			}
			



			
		}

		


		def GenerateGraph : Graph = {


			val (f1, e1, v1) = firstVectors
			val es = orbit(f1, e1, 5)
			val vs = orbit(f1, v1, 5)
			val faceArr  = new Array[Face](12)
			val edgeList = new ListBuffer[Edge]()
			val vertList = new ListBuffer[Vertex]()

			// permutations for rotating clockwise around a face / edge / vertex


			val face0 = new Face(0, p, 0)
			face0.faceMirror = Permutations.m4

			val facePerm0 = Permutations.f0
			
			face0.twistPerm = facePerm0

			// println(facePerms(0).fixedPts)



			face0.pt = f1
			faceArr(0) = face0
			for (i <- 1 until 12){
				faceArr(i) = new Face(i, p, -1)
			}
			

			for (i <- 0 until 12){
				for (j <- 0 until 5){
					faceArr(i).faces(j) = faceArr(dodecaNbrs(i)(j))
				}
			}

			for (i <- 0 until 5){
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
				val oppFace = faceArr(dodecaNbrs(0)(i))
				newE.setFace(1, oppFace)
				oppFace.setEdge(0, newE)
				oppFace.setFace(0, face0)
				oppFace.pt = new Vector3(face0.pt).rotate(newE.pt, 180)
				
				val rotPerm = facePerm0^i 
				val edgePerm = rotPerm * Permutations.e0 * rotPerm.inv
				newE.twistPerm = edgePerm
				newV.twistPerm = rotPerm * Permutations.v0 * rotPerm.inv
				oppFace.faceMirror = edgePerm * face0.faceMirror * edgePerm
				oppFace.twistPerm = edgePerm * face0.twistPerm * edgePerm 

				// println(edgePerm(19))

				// println(facePerms(dodecaNbrs(0)(i)).fixedPts)

			}
			
			for (i <- 0 until 6){
				faceArr(i+6).faceMirror = faceArr(i).faceMirror
				faceArr(i+6).twistPerm = faceArr(i).twistPerm.inv
			}

			for (i <- 0 until 12){
				for (j <- 0 until 5){
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
				for (i <- 1 until 12){
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
				// println("loop")

			}
			// println("done")

			val edgeArr = edgeList.toArray
			for (i <- 0 until edgeArr.length){
				edgeArr(i).id = i
			}
			val vertArr = vertList.toArray
			for (i <- 0 until vertArr.length){
				vertArr(i).id = i
			}

			for (i <- 0 until faceArr.length){
				faceArr(i).oppCell = faceArr(i).faceMirror(0)
				faceArr(i).generateStickers
			}


			


				
			new Graph(faceArr, edgeArr, vertArr, new Vector3, 0, 0, false)
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
			val vID = startFace.vertexScore._2
			val v0 = startFace.getVertex(vID)
			val perm0 = v0.twistPerm
			val pt0 = v0.pt
			var offset = 1
			while (startFace.vertexScore._1 < p){
				val offsetPerm = startFace.twistPerm^offset
				val nextV = startFace.getVertex(vID + offset)
				if (nextV.pt == null){
					nextV.pt = new Vector3(pt0).rotate(startFace.pt, -offset*72)
					nextV.twistPerm = offsetPerm * perm0 * offsetPerm.inv
				}
				offset += 1
			}
		}

		def orbitEdge(startFace:Face, edgeList:ListBuffer[Edge]){
			val eID = startFace.edgeScore._2
			val e0 = startFace.getEdge(eID)
			val perm0 = e0.twistPerm
			val pt0 = e0.pt
			var offset = 1
			while (startFace.edgeScore._1 < p){
				val offsetPerm = startFace.twistPerm^offset
				val id = eID + offset
				val nextE = startFace.getEdge(id)
				if (nextE == null){
					val offsetPerm = startFace.twistPerm^offset
					val newE = new Edge
					newE.setFace(0, startFace)
					newE.setFace(1, startFace.getFace(id))
					edgeList += newE
					newE.twistPerm = offsetPerm * perm0 * offsetPerm.inv
					newE.pt = new Vector3(pt0).rotate(startFace.pt, -offset*72)
					startFace.setEdge(id, newE)
					val nbFace = startFace.getFace(id)
					nbFace.setEdge(nbFace.findFace(startFace), newE)
				}
				offset += 1
			}
		}

		def orbitFace(startFace:Face){
			val firstF = startFace.faceScore._2
			val f0 = startFace.getFace(firstF)
			val perm0 = f0.twistPerm
			val pt0 = f0.pt
			var offset = 1
			while (startFace.faceScore._1 < p){
				val nextF = startFace.getFace(firstF + offset)
				if (nextF.pt == null){
					nextF.pt = new Vector3(pt0).rotate(startFace.pt, -offset*72)
				}
				offset += 1
			}
		}


		

		class Node(var pt:Vector3, var id:Int, val size:Int){

			val faces = new Array[Face](size)

			override def toString(): String = "n"+id

			
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

			var twistPerm = Permutations.Identity

			def TwistFn(dir:Int) : Int => Int = ((twistPerm^dir)(_))

			def getGrips(grip:Int) : Set[Int] = Set(grip)


			
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

			var faceMirror = Permutations.Identity

			def generateStickers {
				val ridgeMidpts = new Array[Vector3](size)
				val edgeMidpts = Array.ofDim[Vector3](size,2)

				for (i <- 0 until size){
					ridgeMidpts(i) = interpolate(verts(i).pt, pt, cutDepth*.75f)
					ridgePts(2*i) = interpolate(verts(i).pt, ridgeMidpts(i), 1/stickerSize)
					edgeMidpts(i)(0) = interpolate(verts(i).pt, edges(i).pt, cutDepth*.5)
					edgeMidpts(i)(1) = interpolate(getVertex(i+1).pt, edges(i).pt, cutDepth*.5)
				}

				for (i <- 0 until size){
					ridgePts(2*i+1) = interpolate(ridgePts(2*i), ridgePts(mod(2*i+2, 2*size)))
					val (e,v) = (edges(i).pt, verts(i).pt)
					vertPts(i)(0) = v.cpy()
					vertPts(i)(1) = interpolate(v, edgeMidpts(i)(0), stickerSize)
					vertPts(i)(2) = interpolate(v, ridgeMidpts(i), stickerSize)
					vertPts(i)(3) = interpolate(v, edgeMidpts(mod(i-1,size))(1), stickerSize)
				}

				for (i <- 0 until size){
					val (e,v, v2) = (edges(i).pt, verts(i).pt, verts((i+1)%size).pt)

					edgePts(i)(0) = e.cpy()
					edgePts(i)(1) = interpolate(v2, edgeMidpts(i)(1), 1/stickerSize)
					edgePts(i)(2) = interpolate(vertPts((i+1)%size)(1), vertPts((i+1)%size)(2), 1/(stickerSize*stickerSize))
					edgePts(i)(4) = interpolate(vertPts(i)(3), vertPts(i)(2), 1/(stickerSize*stickerSize))
					edgePts(i)(5) = interpolate(v, edgeMidpts(i)(0), 1/stickerSize)

					edgePts(i)(3) = interpolate(edgePts(i)(2), edgePts(i)(4))

				}

				val (midpt, pts) = getPolygon
				val f = mobiusScalarMultiply(centerSize, _)
				centerMidpt = f(midpt)
				centerPts = pts.map(f)



			}

			// override def TwistFn(dir:Int) : (Int => Int) = {

			// 	val map = new HashMap[Int,Int]
			// 	for (i <- 0 until 3){
			// 		for (j <- 1 to 3){
			// 			map.addOne(getVertex(i).getOffsetFace(this, j).oppCell, getVertex(i+dir).getOffsetFace(this, j).oppCell)
			// 		}
			// 	}
			// 	return x => map.get(x) match {
			// 		case None => x
			// 		case Some(value) => value
			// 	}
				
			// }

			override def toString(): String = "f"+id
			
			



			

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

			def findEdge(e:Edge) : Int = {
				for (i <- 0 until size){
						if (edges(i) == e){
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

			override def getGrips(grip: Int): Set[Int] = Set(grip, oppCell)


		}

			

		class Edge(i:Int = -1, vec:Vector3 = null) extends Node(vec, i, 2){
			// override def TwistFn(dir:Int) : (Int => Int) = {

			// 	val map = new HashMap[Int,Int]
			// 	val (f0, f1) = (getFace(0), getFace(dir))
			// 	val (v0, v1) = (f0.getVertex(f0.findEdge(this)), f1.getVertex(f1.findEdge(this)))
			// 	val (i0, i1) = (v0.findFace(f0), v1.findFace(f1))
			// 	for (j <- 0 until 4){
			// 		val (c0, c1) = (v0.getFace(i0+j).oppCell, v1.getFace(i1+j).oppCell)
			// 		map.addOne(c0,c1)
			// 		map.addOne(c1,c0)
			// 	}
			// 	return x => map.get(x) match {
			// 		case None => x
			// 		case Some(value) => value
			// 	}
				
			// }

			override def getGrips(grip:Int) : Set[Int] = {
				val set = HashSet(grip)
				for (f <- faces){
					set.addOne(f.oppCell)
					set.addOne((f.faceMirror * twistPerm * f.faceMirror)(grip))
				}
				if (set.size != 5){
					println("grip error" + set.size)
				}
				return set.toSet
			} 

			override def toString(): String = "e"+id
		}

		class Vertex(i:Int = -1, val q:Int, vec:Vector3 = null) extends Node(vec, i, q){
			// override def TwistFn(dir:Int) : (Int => Int) = {

			// 	val map = new HashMap[Int,Int]
			// 	for (i <- 0 until 5){
			// 		val (f1, f2) = (getFace(i), getFace(i+dir))
			// 		val (i1, i2) = (f1.findVertex(this), f2.findVertex(this))
			// 		map.addOne(f1.oppCell, f2.oppCell)
			// 		map.addOne(f1.getFace(i1+1).oppCell, f2.getFace(i2+1).oppCell)

			// 	}
			// 	return x => map.get(x) match {
			// 		case None => x
			// 		case Some(value) => value
			// 	}
				
			// }

			override def getGrips(grip:Int) : Set[Int] = {
				val set = HashSet(grip)
				for (f <- faces){
					set.addOne(f.oppCell)
					set.addOne((f.faceMirror * twistPerm * f.faceMirror)(grip))
					set.addOne((f.faceMirror * twistPerm.inv * f.faceMirror)(grip))

				}
				if (set.size != 10){
					println("grip error" + set.size)
				}
				return set.toSet
			}

			override def toString(): String = "v"+id
		}


		val hemiDodecaNbrs = Array(
			Array(1,2,3,4,5),
			Array(0,5,9,10,2),
			Array(0,1,10,11,3),
			Array(0,2,11,7,4),
			Array(0,3,7,8,5),
			Array(0,4,8,9,1)
			
		)

		val dodecaNbrs = hemiDodecaNbrs ++ hemiDodecaNbrs.map(_.map(x => (x+6)%12).reverse)
		// println(icosaNbrs.map(_.mkString(",")).mkString("\n"))

}