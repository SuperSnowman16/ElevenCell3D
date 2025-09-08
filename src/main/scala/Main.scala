



import com.badlogic.gdx._
import com.badlogic.gdx.backends.lwjgl3._
import com.badlogic.gdx.graphics._
import com.badlogic.gdx.graphics.g3d._
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.utils._
import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.math.Vector3
import com.badlogic.gdx.graphics.g3d.attributes.IntAttribute
// import Maths.atanh
import java.lang.Math._ 
import java.lang.Math.min

import com.badlogic.gdx.graphics.g3d.environment.DirectionalLight
import com.badlogic.gdx.math.Quaternion
import com.badlogic.gdx.math.MathUtils
// import Maths.mod
// import Colours.DistinctColorArray
import java.awt.{Color => AWTColor}
import scala.collection.mutable.ListBuffer
import Maths3D._
import com.badlogic.gdx.graphics.g3d.attributes.BlendingAttribute
import com.badlogic.gdx.math.Vector2
import Maths3D.Hyperbolic.interpolate
import MyGame.{stickerSize, depth}
import Maths3D.Hyperbolic.lerp
import Graphs.Graph


object MyGame {
	val stickerSize = .8f
	val depth = .7f
	def main(args: Array[String]): Unit = {
		val config = new Lwjgl3ApplicationConfiguration()
		config.setTitle("11 Cell")
		config.setWindowedMode(800, 600)
		config.setBackBufferConfig(8, 8, 8, 8, 16, 0, 4)
		new Lwjgl3Application(new MyGame(), config)
	}
}

class MyGame extends ApplicationAdapter {
	private var modelBatch: ModelBatch = _
	private var environment: Environment = _
	private var camera: PerspectiveCamera = _
	private var camController: CameraInputController = _

	private var model: Model = _
	private var instance: ModelInstance = _

	var orientation = new Quaternion()
	var sensitivity = 0.2f
	var lastX = 0
	var lastY = 0
	var rotating = false

	val graph = Graphs.GenerateGraph
	val faceArr = graph.faces
	val graphs = new Array[Graph](21)
	for (i <- 0 until 20){
		val graph2 = graph.mirror(i)
		val offset = graph2.midpoint.scl(.75f)
		graph2.transform(_.add(offset))
		graphs(i) = graph2
	}
	graphs(20) = graph

	val cells = graphs.take(10).prepended(graph)

	val state = new State(cells)


	override def create(): Unit = {

		// Set up camera
		camera = new PerspectiveCamera(67, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
		camera.position.set(0f, 0f, 3f) 
		camera.lookAt(0f, 0f, 0f)
		camera.near = 0.1f
		camera.far = 100f
		camera.update()

		// camController = new CameraInputController(camera)
		// camController.rotateAngle = -360
		// camController.
		// Gdx.input.setInputProcessor(camController)
		// Gdx.input.setInputProcessor(new MyInputProcessor(camera))


		// Lighting environment
		environment = new Environment()
		val lightIntensity = 0.8f
		environment.set(new ColorAttribute(ColorAttribute.AmbientLight, lightIntensity, lightIntensity, lightIntensity, 1f))

		// environment.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.3f, 0.3f, 0.3f, 1f))
		environment.add(new DirectionalLight().set(1f-lightIntensity, 1f-lightIntensity, 1f-lightIntensity, 0f, 0f, -1f))

		modelBatch = new ModelBatch()

		// Build cube from triangles
		val builder = new ModelBuilder()
		builder.begin()
		val material = new Material(
			new BlendingAttribute(true, 1f)
		)
		material.set(IntAttribute.createCullFace(GL20.GL_NONE))
		val meshPartBuilder = builder.part(
			"triangles",
			GL20.GL_TRIANGLES,
			Usage.Position | Usage.Normal | Usage.ColorUnpacked,
			material
		)

		val material2 = new Material(ColorAttribute.createDiffuse(Color.BLACK))

		val meshLineBuilder = builder.part(
			"lines",
			GL20.GL_LINES,
			Usage.Position,
			material2
		)

		// Cube corners
		val v000 = new Vector3(-1, -1, -1)
		val v001 = new Vector3(-1, -1,  1)
		val v010 = new Vector3(-1,  1, -1)
		val v011 = new Vector3(-1,  1,  1)
		val v100 = new Vector3( 1, -1, -1)
		val v101 = new Vector3( 1, -1,  1)
		val v110 = new Vector3( 1,  1, -1)
		val v111 = new Vector3( 1,  1,  1)

		// --- Each face: 2 triangles ---

		// val o = new Vector3
		// val (f1,e1,v1) = ElevenCell.firstVectors
		// val es = ElevenCell.orbit(f1, e1, 3)
		// val vs = ElevenCell.orbit(f1, v1, 3)
		// // // Front (z = +1)
		meshPartBuilder.setColor(Color.ORANGE)
		// meshLineBuilder.setColor(Color.BLACK)

		def toGdxColor(c: AWTColor): Color = {
			new Color(c.getRed / 255f, c.getGreen / 255f, c.getBlue / 255f, 1f)
		}

		val colors = Array(
			AWTColor.decode("#ffffff"),
			AWTColor.decode("#ff0000"),
			AWTColor.decode("#ff9900"),
			AWTColor.decode("#ffff00"),
			AWTColor.decode("#00ff00"),
			AWTColor.decode("#00ffff"),
			AWTColor.decode("#0080ff"),
			AWTColor.decode("#0000ff"),
			AWTColor.decode("#9900ff"),
			AWTColor.decode("#ff00ff"),
			AWTColor.decode("#333333"),
			AWTColor.decode("#000000")
			
		).map(toGdxColor)
		

		

		
		def DrawPolygon(pts:Array[Vector3]){
			val p = pts(0)
				meshLineBuilder.line(p, pts(1))
			for(i <- 1 until pts.size-1){
				meshPartBuilder.triangle(p, pts(i), pts(i+1))
				meshLineBuilder.line(pts(i), pts(i+1))
			}
			
			
		}

		
		for (g <- graphs){
			val cell = g.color
			for (f <- g.faces){
				val ridgeColor = colors(state.get(cell, Set(cell, f.oppCell)))
				meshPartBuilder.setColor(ridgeColor)
				val pts = f.ridgePts
				meshPartBuilder.triangle(pts(0),pts(1),pts(2))
				for (i <- 0 until 3){
					val edgeColor = colors(state.get(cell, f.edges(i).faces.map(_.oppCell).appended(cell).toSet))
					meshPartBuilder.setColor(edgeColor)
					DrawPolygon(f.edgePts(i))
					val vertColor = colors(state.get(cell, f.verts(i).faces.map(_.oppCell).appended(cell).toSet))
					meshPartBuilder.setColor(vertColor)
					DrawPolygon(f.vertPts(i))
				}
			}
		}
		
		


		model = builder.end()
		instance = new ModelInstance(model)

		Gdx.input.setInputProcessor(new InputAdapter {

			val clickThreshold = 5f // pixels
			var dragDetected = false
			var touchStartX = 0f
			var touchStartY = 0f
			override def touchDown(screenX: Int, screenY: Int, pointer: Int, button: Int): Boolean = {
				rotating = true
				lastX = screenX
				lastY = screenY

				touchStartX = screenX
				touchStartY = screenY
				dragDetected = false

				true
			}


			override def touchDragged(screenX: Int, screenY: Int, pointer: Int): Boolean = {
				if (rotating) {
					val dx = (screenX - lastX) * sensitivity
					val dy = (screenY - lastY) * sensitivity

					// mark as drag if moved enough
					if (!dragDetected && Vector2.dst(touchStartX, touchStartY, screenX, screenY) > clickThreshold){
						dragDetected = true
					}
					// build incremental quaternions
					val qYaw   = new Quaternion(Vector3.Y, dx)  // rotate around Y
					val qPitch = new Quaternion(Vector3.X, dy)  // rotate around X

					// update orientation (order matters!)
					orientation.mulLeft(qYaw).mulLeft(qPitch)

					// apply orientation to the cube
					instance.transform.idt().rotate(orientation)

					lastX = screenX
					lastY = screenY
				}
				true
			}

			override def touchUp(screenX: Int, screenY: Int, pointer: Int, button: Int): Boolean = {
				rotating = false

				// Only pick if it was a click, not a drag
				// if (!dragDetected) {
				// 	val ray = camera.getPickRay(screenX.toFloat, screenY.toFloat)
				// 	pickFace(ray, instances) match {
				// 		case Some(hit) =>
				// 		println(s"Clicked face '${hit.faceId}' of dodecahedron at ${hit.point}")
				// 		case None =>
				// 		println("No hit")
				// 	}
				// }

				true
			}
		})
	}

	
	

	override def render(): Unit = {
		Gdx.gl.glViewport(0, 0, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
		Gdx.gl.glClearColor(0, 0, 0, 1)
		Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)

		camera.update()

		// camController.target.set(0f, 0f, 0f)
		// camController.update()

		modelBatch.begin(camera)
		modelBatch.render(instance, environment)
		modelBatch.end()
	}

	override def dispose(): Unit = {
		modelBatch.dispose()
		model.dispose()
	}

	// def drawTriangle(meshPartBuilder:MeshPartBuilder, v1: Vector3, v2: Vector3, v3: Vector3, color:Color){
		
	// }

	def makeCylinderBetween(p1: Vector3, p2: Vector3, radius: Float, color: Color): ModelInstance = {
		val dir = new Vector3(p2).sub(p1)         // direction vector
		val length = dir.len()                    // cylinder height
		dir.nor()

		// Create a cylinder aligned with Y-axis
		val modelBuilder = new ModelBuilder()
		val material = new Material(ColorAttribute.createDiffuse(color))
		val model: Model = modelBuilder.createCylinder(
			radius * 2, length, radius * 2, 32,
			material,
			Usage.Position | Usage.Normal
		)
		val instance = new ModelInstance(model)

		// Rotation: align cylinder's local Y with dir
		val up = Vector3.Y.cpy()
		val q = new Quaternion().setFromCross(up, dir)
		instance.transform.set(q)

		// Move to midpoint
		val mid = new Vector3(p1).add(p2).scl(0.5f)
		instance.transform.setTranslation(mid)

		instance
	}

	def computeNormal(v1: Vector3, v2: Vector3, v3: Vector3): Vector3 = {
		val edge1 = v2.cpy().sub(v1)
		val edge2 = v3.cpy().sub(v1)
		edge1.crs(edge2).nor()  // cross product → perpendicular unit vector
	}

	override def resize(width: Int, height: Int): Unit = {
		camera.viewportWidth = width.toFloat
		camera.viewportHeight = height.toFloat
		camera.update()
	}
}








