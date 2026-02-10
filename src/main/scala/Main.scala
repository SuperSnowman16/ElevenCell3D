



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
import Main._
import Graphs.Graph
import com.badlogic.gdx.math.collision.Ray
import scala.jdk.CollectionConverters._
import com.badlogic.gdx.math.Intersector
import com.badlogic.gdx.math.Matrix4
import Graphs.Node
import Graphs.Face
import scala.collection.mutable.HashMap
import com.badlogic.gdx.Input.Buttons
import com.badlogic.gdx.graphics.g3d.attributes.DepthTestAttribute
import com.badlogic.gdx.Input.Keys
import scala.util.Random
import Maths3D.Mobius.mobiusScalarMultiply
import com.badlogic.gdx.graphics.glutils.HdpiMode
import com.badlogic.gdx.utils.viewport.FitViewport
import javax.swing.JOptionPane
import javax.swing.SwingUtilities
import Maths3D.Mobius.PoincareToOrthographic
import java.lang.reflect.InvocationTargetException
import javax.swing.JFileChooser
import java.nio.file.{Files, Paths, StandardOpenOption}
import java.nio.charset.StandardCharsets
import java.awt.Toolkit
import javax.swing.JDialog
import java.util.concurrent.Executors


object Main {

	@volatile var isDirty = false
	// flag used to prevent multiple choosers at once
	@volatile private var isExitDialogOpen = false
	private val defaultDir = Paths.get("Saves")
	Files.createDirectories(defaultDir)

	val stickerSize = .8f
	val cutDepth = .7f
	val cellSize = .6f
	val centerSize = .5f
	val transparency = .8f
	// val outerCellScaling = 1f
	
	def main(args: Array[String]): Unit = {
		val config = new Lwjgl3ApplicationConfiguration()
		config.setTitle("11 Cell")
		config.setWindowedMode(800, 600)
	
		val app = new Main()

		config.setResizable(true)
		config.setHdpiMode(HdpiMode.Pixels)
		config.setBackBufferConfig(8, 8, 8, 8, 16, 0, 4)
		config.setWindowListener(new Lwjgl3WindowAdapter {
			override def closeRequested(): Boolean = {
				app.handleCloseRequest()
			}
		})
		new Lwjgl3Application(app, config)
	}
}

class Main extends ApplicationAdapter {
	private var modelBatch: ModelBatch = _
	private var environment: Environment = _
	private var camera: PerspectiveCamera = _
	private var camController: CameraInputController = _

	var viewport : FitViewport = _

	private var model: Model = _
	private var instances = Array[ModelInstance]()

	def toGdxColor(c: AWTColor): Color = {
		new Color(c.getRed / 255f, c.getGreen / 255f, c.getBlue / 255f, transparency)
	}

	private val saveExecutor = Executors.newSingleThreadExecutor()

	


	
	val colors = Array(
		AWTColor.decode("#ffffff"),
		AWTColor.decode("#ff0000"),
		AWTColor.decode("#ff7b00"),
		AWTColor.decode("#ffff00"),
		AWTColor.decode("#00ff00"),
		AWTColor.decode("#00ffff"),
		AWTColor.decode("#0080ff"),
		AWTColor.decode("#0000ff"),
		AWTColor.decode("#9900ff"),
		AWTColor.decode("#ff00ff"),
		AWTColor.decode("#686868"),
		AWTColor.decode("#000000")
		
	).map(toGdxColor) 

	var orientation = new Quaternion()
	var sensitivity = 0.2f
	var lastX = 0
	var lastY = 0
	var rotating = false
	var show3rdLayer = true

	val graph = Graphs.GenerateGraph
	// graph.transform(mobiusScalarMultiply(0.8f, _))
	// graph.transform(_.scl(0.8f))
	
	val faceArr = graph.faces
	var graphs = new Array[Graph](21)
	for (i <- 0 until 20){
		val graph2 = graph.mirror(i)
		// val offset = graph2.midpoint.scl(cellSpacing)
		// graph2.transform(_.scl(outerCellScaling).add(offset))
		graphs(i) = graph2
	}
	graphs(20) = graph

	val layer3 = new Array[Graph](60)

	for (i <- 0 until 20){
		val g = graphs(i)
		for (j <- 0 until 3){
			val f0 = g.faces(i)
			val f1 = f0.getFace(j)
			val f2 = f1.getOffsetFace(f0, 1)
			// val f3 = f2.getOffsetFace(f1, -1)
			layer3(3*i + j) = g.mirror(f2.id)
		}
	}

	graphs = graphs ++ layer3

	graphs.foreach(g => g.transform(x => interpolate(g.midpoint, x, cellSize)))

	// graphs.foreach(g => g.transform(PoincareToOrthographic(_)))

	// val f = rotateAroundLine(_, graphs(0).midpoint, graphs(0).faces(1).centerMidpt, 120)
	// graphs.foreach(g => g.transform(translateToOrigin(_, new Vector3(.5f,0,0))))
	// graphs.foreach(g => g.transform(f))
	
	for(i <- 0 until graphs.length){
		graphs(i).id = i
	}

	instances = new Array[ModelInstance](graphs.length)


	val cells = graphs.take(10).appended(graph)

	val state = new State(cells, this)

	def dumpInstanceInfo(): Unit = {
	println("=== Instance parts ===")
	for {
		instance <- instances
		node <- instance.nodes.asScala
		part <- node.parts.asScala
	} {
		val meshPart = part.meshPart
		val mesh = meshPart.mesh
		val id = meshPart.id
		val hasColorPacked = mesh.getVertexAttribute(Usage.ColorPacked) != null
		val hasColorUnpacked = mesh.getVertexAttribute(Usage.ColorUnpacked) != null
		val hasVertexColors = hasColorPacked || hasColorUnpacked

		val mat = part.material
		val diffuseAttr = Option(mat.get(com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute.Diffuse))
		val diffuseColorStr = diffuseAttr.map(_.asInstanceOf[com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute].color.toString).getOrElse("none")

		println(s"part id='$id', vertices=${mesh.getNumVertices}, vertexColors=$hasVertexColors, diffuse=$diffuseColorStr")
	}
	println("=====================")
	}

	def randomMove {
		val color = Random.nextInt(11)
		val cell = cells(color)
		var graphID = color
		if (color == 10){
			graphID = 20
		}
		val twistType = Random.nextInt(3)
		val pieces = twistType match {
			case 0 => cell.faces
			case 1 => cell.edges
			case 2 => cell.verts
		}
		val piece = pieces(Random.nextInt(pieces.size))

		val twistStr = "1t"+graphID+piece.toString()


		
		state.Twist(color, piece.TwistFn(1))
		state.scrambleList.addOne(twistStr)

		isDirty = true
	}

	// def ensureUniqueMaterials(): Unit = {
	// 	for {
	// 		instance <- instances
	// 		node <- instance.nodes.asScala
	// 		part <- node.parts.asScala
	// 	} {
	// 		part.material = new com.badlogic.gdx.graphics.g3d.Material(part.material) // clone
	// 	}
	// }


	def parseMeshID(id: String): (Int, Node) = id match {
		case s"c${cell}m${face}" => (cell.toInt, graphs(cell.toInt).faces(face.toInt))
		case s"c${cell}f${face}" => (cell.toInt, graphs(cell.toInt).faces(face.toInt))
		case s"c${cell}e${edge}" => (cell.toInt, graphs(cell.toInt).edges(edge.toInt))
		case s"c${cell}v${vert}" => (cell.toInt, graphs(cell.toInt).verts(vert.toInt))

		case _ => throw new Exception()
	}

	def meshIDtoGrip(id: String): (Int, Set[Int]) = id match {
		case s"c${cell}m${face}" => 
			val color = graphs(cell.toInt).color
			(color, Set(color))
		case s"c${cell}f${face}" => 
			val color = graphs(cell.toInt).color
			(color, Set(color, graphs(cell.toInt).faces(face.toInt).oppCell))
		case _ => 
			val (cell, node) = parseMeshID(id)
			val color = graphs(cell).color
			(color, node.faces.map(f => f.oppCell).appended(color).toSet)
	}

	val materialMap = new HashMap[String, Material]


	def CenterCell(id:Int) : Int => Int = {


		val map = new HashMap[Int, Int]()

		val mid = graphs(20)
		val newMid = graphs(id)
		map.addOne(mid.color, newMid.color)
		map.addOne(newMid.color, mid.color)


		val f0 = mid.faces(id) 
		for (v <- f0.verts){
			val f1 = v.getOffsetFace(f0, 2)
			val f2 = v.getOffsetFace(f0, -2)
			map.addOne(f1.oppCell, f2.oppCell)
			map.addOne(f2.oppCell, f1.oppCell)
		}

		return x => map.get(x) match {
			case Some(value) => value
			case None => x
		}

	}

	

	def updateColors {
		// for ((k,v) <- materialMap){
		// 	val color = state.get(meshIDtoGrip(k))
		// 	// println(color)
		// 	val d = v.get(ColorAttribute.Diffuse).asInstanceOf[ColorAttribute]
		// 	d.color.set(color)
		// }

		for {
			instance <- instances
			node <- instance.nodes.asScala
			part <- node.parts.asScala
			if part.meshPart.id != "lines"
		} {
			val id = part.meshPart.id
			val mat = part.material
			val newColor = colors(state.get(meshIDtoGrip(id)))
		
			// Ensure material actually has a ColorAttribute.Diffuse — if not, add one.
			val attr = mat.get(ColorAttribute.Diffuse)
			
			if (attr == null) {
			// create and add a ColorAttribute (preserves any blending attribute already present)
			mat.set(ColorAttribute.createDiffuse(newColor))
			// println(s"Added new diffuse attribute to part '$faceId'")
			} else {
			// mutate the existing ColorAttribute so we don't replace BlendingAttribute etc.
			val ca = attr.asInstanceOf[ColorAttribute]
			// println(s"Before update for '$faceId': ${ca.color}")
			ca.color.set(newColor)
			// println(s"After update for '$faceId': ${ca.color}")
			}
		}

	}

	// def updateFaceColorByPart(faceId: String, newColor: Color): Unit = {
	// 	var found = false
	// 	for {
	// 		instance <- instances
	// 		node <- instance.nodes.asScala
	// 		part <- node.parts.asScala
	// 		// if part.meshPart.id == faceId
	// 	} {
	// 		found = true
	// 		val mat = part.material
		
	// 		// Ensure material actually has a ColorAttribute.Diffuse — if not, add one.
	// 		val attr = mat.get(ColorAttribute.Diffuse)
	// 		if (attr == null) {
	// 		// create and add a ColorAttribute (preserves any blending attribute already present)
	// 		mat.set(ColorAttribute.createDiffuse(newColor))
	// 		println(s"Added new diffuse attribute to part '$faceId'")
	// 		} else {
	// 		// mutate the existing ColorAttribute so we don't replace BlendingAttribute etc.
	// 		val ca = attr.asInstanceOf[ColorAttribute]
	// 		println(s"Before update for '$faceId': ${ca.color}")
	// 		ca.color.set(newColor)
	// 		println(s"After update for '$faceId': ${ca.color}")
	// 		}
	// 	}

	// 	if (!found) println(s"updateFaceColorByPart: no part found with id '$faceId'")
	// }



	override def create(): Unit = {

		// Set up camera
		camera = new PerspectiveCamera(67, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
		camera.position.set(0f, 0f, 2f) 
		camera.lookAt(0f, 0f, 0f)
		camera.near = 0.1f
		camera.far = 100f
		camera.update()

		// viewport = new FitViewport(800,600,camera)
		// viewport.apply()

		// camController = new CameraInputController(camera)
		// camController.rotateAngle = -360
		// camController.
		// Gdx.input.setInputProcessor(camController)
		// Gdx.input.setInputProcessor(new MyInputProcessor(camera))


		// Lighting environment
		environment = new Environment()
		val lightIntensity = .9f
		environment.set(new ColorAttribute(ColorAttribute.AmbientLight, lightIntensity, lightIntensity, lightIntensity, 1f))

		// environment.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.3f, 0.3f, 0.3f, 1f))
		environment.add(new DirectionalLight().set(1f-lightIntensity, 1f-lightIntensity, 1f-lightIntensity, 0f, 0f, -1f))

		modelBatch = new ModelBatch()

		// Build cube from triangles
		val builder = new ModelBuilder()
		builder.begin()
		// val material = new Material(
		// 	new BlendingAttribute(true, 1f)
		// )
		// material.set(IntAttribute.createCullFace(GL20.GL_NONE))
		val attrs = Usage.Position | Usage.Normal 
		

		val material2 = new Material(ColorAttribute.createDiffuse(Color.BLACK))

		var meshLineBuilder = builder.part(
			"lines",
			GL20.GL_LINES,
			Usage.Position,
			material2
		)

		
		

		

		// Draws a polygon connecting the first point to each subsequent consecutive pair with a triangle 
		def DrawPolygon(pts:Array[Vector3], color:Color, name:String){ 
			val mat = materialMap.get(name) match {
				case Some(value) => value
				case None => 
					val m = new Material(
					ColorAttribute.createDiffuse(color),
					new BlendingAttribute(true, 1f)
					// new DepthTestAttribute(false)
				)
				m.set(IntAttribute.createCullFace(GL20.GL_NONE))
				materialMap.addOne(name, m)
				m
			}
			val part = builder.part(name, GL20.GL_TRIANGLES, attrs, mat)
			// part.setColor(color)
			val p = pts(0)
				meshLineBuilder.line(p, pts(1))
				meshLineBuilder.line(p, pts.last)

			for(i <- 1 until pts.size-1){
				part.triangle(p, pts(i), pts(i+1))
				meshLineBuilder.line(pts(i), pts(i+1))
			}
			
			
		}

		// Draws a polygon connecting the midpoint to all the other points in order, then back to the first
		def DrawPolygon2(midp:Vector3, pts:Array[Vector3], color:Color, name:String){ 
			val mat = materialMap.get(name) match {
				case Some(value) => value
				case None => 
					val m = new Material(
					ColorAttribute.createDiffuse(color),
					new BlendingAttribute(true, 1f)
					// new DepthTestAttribute(false)
				)
				m.set(IntAttribute.createCullFace(GL20.GL_NONE))
				materialMap.addOne(name, m)
				m
			}
			val part = builder.part(name, GL20.GL_TRIANGLES, attrs, mat)
			// part.setColor(color)
			for(i <- 0 until pts.size-1){
				part.triangle(midp, pts(i), pts(i+1))
				meshLineBuilder.line(pts(i), pts(i+1))
			}
			part.triangle(midp, pts.last, pts(0))
			meshLineBuilder.line(pts.last, pts(0))
		}



		builder.end()
		for (j <- 0 until graphs.length){
			// if (j > 0){
				builder.begin()
			// }
			meshLineBuilder = builder.part(
				"lines",
				GL20.GL_LINES,
				Usage.Position,
				material2
			)
			
			val g = graphs(j)
			val cell = g.color
			for (f <- g.faces){

				
				DrawPolygon2(f.centerMidpt, f.centerPts, colors(cell), "c"+g.id+"m"+f.id)

				val ridgeColor = colors(state.get(cell, Set(cell, f.oppCell)))
				DrawPolygon2(f.pt, f.ridgePts, ridgeColor, "c"+g.id+"f"+f.id)
				
				for (i <- 0 until 3){
					val edgeColor = colors(state.get(cell, f.edges(i).faces.map(_.oppCell).appended(cell).toSet))
					DrawPolygon(f.edgePts(i), edgeColor, "c"+g.id+"e"+f.edges(i).id)
					val vertColor = colors(state.get(cell, f.verts(i).faces.map(_.oppCell).appended(cell).toSet))
					DrawPolygon(f.vertPts(i), vertColor, "c"+g.id+"v"+f.verts(i).id)
				}
			}
			val model = builder.end()

			instances(j) = new ModelInstance(model) 
		}

		// ensureUniqueMaterials()

	// state.Twist(10, cells(10).faces(0).FaceTwistFn(1))

	

		
		



		Gdx.input.setInputProcessor(defaultInputProcessor)
	}

	
	

	override def render(): Unit = {




		Gdx.gl.glViewport(0, 0, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
		Gdx.gl.glClearColor(0, 0, 0, 1)
		Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)

		camera.update()
		// viewport.apply()
		

		// camController.target.set(0f, 0f, 0f)
		// camController.update()

		modelBatch.begin(camera)
		val cellNum = show3rdLayer match {
			case false => 21
			case true => 81
		}
		for (i <- 0 until cellNum){	
			modelBatch.render(instances(i), environment)
		}
		modelBatch.end()


		
	}

	override def dispose(): Unit = {
		modelBatch.dispose()
		// model.dispose()
	}


	

	

	/** Called by Lwjgl3WindowAdapter.closeRequested(). */
	def handleCloseRequest(): Boolean = {
    if (!isDirty) return true
    if (isExitDialogOpen) return false

    isExitDialogOpen = true

    // Install modal input blocker on the libGDX thread
    Gdx.app.postRunnable(new Runnable {
      override def run(): Unit = {
        Gdx.input.setInputProcessor(modalInputBlocker)
      }
    })

    // Run Swing confirm + modal JFileChooser on the EDT
    SwingUtilities.invokeLater(new Runnable {
      override def run(): Unit = {
        try {
          // 1) Confirm dialog
          val choice = JOptionPane.showConfirmDialog(
            null,
            "You have unsaved changes. Save before exiting?",
            "Unsaved Changes",
            JOptionPane.YES_NO_CANCEL_OPTION
          )

          choice match {
            case JOptionPane.YES_OPTION =>
              // 2) Modal JDialog containing JFileChooser
              val dialog = new JDialog(null: java.awt.Frame, "Choose save file", java.awt.Dialog.ModalityType.APPLICATION_MODAL)
              val chooser = new JFileChooser()
              chooser.setCurrentDirectory(defaultDir.toFile)

              chooser.addActionListener(new java.awt.event.ActionListener {
                override def actionPerformed(e: java.awt.event.ActionEvent): Unit = {
                  val cmd = e.getActionCommand
                  if ("ApproveSelection" == cmd || "CancelSelection" == cmd) {
                    dialog.setVisible(false)
                    dialog.dispose()
                  }
                }
              })

              dialog.getContentPane.add(chooser)
              dialog.pack()
              dialog.setAlwaysOnTop(true)
              dialog.setLocationRelativeTo(null)
              dialog.setVisible(true) // blocks EDT until chooser closed

              val selected = chooser.getSelectedFile
              if (selected != null) {
                // write off the EDT
                saveExecutor.submit(new Runnable {
                  override def run(): Unit = {
                    try {
                      state.saveState(selected.getPath())
                      isDirty = false

                      // restore input & exit on libGDX thread
                      Gdx.app.postRunnable(new Runnable {
                        override def run(): Unit = {
                          Gdx.input.setInputProcessor(defaultInputProcessor)
                          isExitDialogOpen = false
                          Gdx.app.exit()
                        }
                      })
                    } catch {
                      case ex: Exception =>
                        ex.printStackTrace()
                        // show error on EDT then restore input
                        JOptionPane.showMessageDialog(null, s"Save failed: ${ex.getMessage}", "Save Error", JOptionPane.ERROR_MESSAGE)
                        Gdx.app.postRunnable(new Runnable {
                          override def run(): Unit = {
                            Gdx.input.setInputProcessor(defaultInputProcessor)
                            isExitDialogOpen = false
                          }
                        })
                    }
                  }
                })
              } else {
                // chooser cancelled -> restore input on libGDX thread
                Gdx.app.postRunnable(new Runnable {
                  override def run(): Unit = {
                    Gdx.input.setInputProcessor(defaultInputProcessor)
                    isExitDialogOpen = false
                  }
                })
              }

            case JOptionPane.NO_OPTION =>
              // Don't save -> restore input + exit
              Gdx.app.postRunnable(new Runnable {
                override def run(): Unit = {
                  Gdx.input.setInputProcessor(defaultInputProcessor)
                  isExitDialogOpen = false
                  Gdx.app.exit()
                }
              })

            case _ => // CANCEL or closed
              Gdx.app.postRunnable(new Runnable {
                override def run(): Unit = {
                  Gdx.input.setInputProcessor(defaultInputProcessor)
                  isExitDialogOpen = false
                }
              })
          }
        } catch {
          case t: Throwable =>
            t.printStackTrace()
            Gdx.app.postRunnable(new Runnable {
              override def run(): Unit = {
                Gdx.input.setInputProcessor(defaultInputProcessor)
                isExitDialogOpen = false
              }
            })
        }
      }
    })

    // Cancel the native close now; we'll exit later if requested
    false
  }

	// def dispose(): Unit = {
	// 	// cleanup
	// }

	// def drawTriangle(meshPartBuilder:MeshPartBuilder, v1: Vector3, v2: Vector3, v3: Vector3, color:Color){
		
	// }

	// def makeCylinderBetween(p1: Vector3, p2: Vector3, radius: Float, color: Color): ModelInstance = {
	// 	val dir = new Vector3(p2).sub(p1)         // direction vector
	// 	val length = dir.len()                    // cylinder height
	// 	dir.nor()

	// 	// Create a cylinder aligned with Y-axis
	// 	val modelBuilder = new ModelBuilder()
	// 	val material = new Material(ColorAttribute.createDiffuse(color))
	// 	val model: Model = modelBuilder.createCylinder(
	// 		radius * 2, length, radius * 2, 32,
	// 		material,
	// 		Usage.Position | Usage.Normal
	// 	)
	// 	val instance = new ModelInstance(model)

	// 	// Rotation: align cylinder's local Y with dir
	// 	val up = Vector3.Y.cpy()
	// 	val q = new Quaternion().setFromCross(up, dir)
	// 	instance.transform.set(q)

	// 	// Move to midpoint
	// 	val mid = new Vector3(p1).add(p2).scl(0.5f)
	// 	instance.transform.setTranslation(mid)

	// 	instance
	// }

	def computeNormal(v1: Vector3, v2: Vector3, v3: Vector3): Vector3 = {
		val edge1 = v2.cpy().sub(v1)
		val edge2 = v3.cpy().sub(v1)
		edge1.crs(edge2).nor()  // cross product → perpendicular unit vector
	}

	override def resize(width: Int, height: Int): Unit = {
		val framebufferWidth  = com.badlogic.gdx.Gdx.graphics.getWidth
		val framebufferHeight = com.badlogic.gdx.Gdx.graphics.getHeight

		camera.viewportWidth  = framebufferWidth.toFloat
		camera.viewportHeight = framebufferHeight.toFloat
		camera.update()
		// viewport.update(width, height, true)
	}


	case class FaceHit(faceId: String, point: Vector3, distance: Float)


	def pickFace(ray: Ray, instances: Array[ModelInstance]): Option[FaceHit] = {
		var closestHit: Option[FaceHit] = None
		var minDst = Float.MaxValue

		val invTransform = new Matrix4(instances(0).transform)
		invTransform.inv()  // invert the model transform

		val localRay = new Ray(ray.origin.cpy(), ray.direction.cpy())
		localRay.mul(invTransform)  // now the ray is in local coordinates
		val tempInstances = show3rdLayer match {
			case true => instances
			case false => instances.take(21)
		}
		for {
			instance <- tempInstances
			node <- instance.nodes.asScala
			part <- node.parts.asScala if !part.meshPart.id.equals("lines")
		} {
			val meshPart = part.meshPart
			val mesh = meshPart.mesh

			val vertices = new Array[Float](mesh.getNumVertices * mesh.getVertexSize / 4)
			mesh.getVertices(vertices)

			val posAttr = mesh.getVertexAttribute(Usage.Position)
			val stride = mesh.getVertexSize / 4
			val posOffset = posAttr.offset / 4

			val indices = new Array[Short](meshPart.size)
			mesh.getIndices(meshPart.offset, meshPart.size, indices, 0)

			for (i <- 0 until meshPart.size by 3) {
			def vertex(idx: Int) = {
				val base = idx * stride + posOffset
				new Vector3(vertices(base), vertices(base + 1), vertices(base + 2))
			}

			val v0 = vertex(indices(i))
			val v1 = vertex(indices(i + 1))
			val v2 = vertex(indices(i + 2))

			val intersection = new Vector3()
			if (Intersector.intersectRayTriangle(localRay, v0, v1, v2, intersection)) {
				val dst = localRay.origin.dst2(intersection)
				if (dst < minDst) {
				minDst = dst
				closestHit = Some(FaceHit(meshPart.id, intersection.cpy(), dst))
				}
			}
			}
		}

		closestHit
	}

	val defaultInputProcessor = new InputAdapter {

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

			override def scrolled(amountX: Float, amountY: Float): Boolean = {
				camera.position.add(0f, 0f, amountY*.1f)
				if (camera.position.z < 0f){
					camera.position.set(0,0,0)
				} 
				return true
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
					instances.map(_.transform.idt().rotate(orientation))

					lastX = screenX
					lastY = screenY
				}
				true
			}

			override def touchUp(screenX: Int, screenY: Int, pointer: Int, button: Int): Boolean = {
				rotating = false

				// Only pick if it was a click, not a drag
				if (!dragDetected) {
					val ray = camera.getPickRay(screenX.toFloat, screenY.toFloat)
					pickFace(ray, instances) match {
						case Some(hit) =>
							val btn = button match {
								case Buttons.LEFT => -1
								case Buttons.RIGHT => 1
								case Buttons.MIDDLE =>
									val (cell, _) = parseMeshID(hit.faceId)
									if (cell < 20){
										val rotStr =  "c"+cell
										state.Rotate(CenterCell(cell))
										state.moveList.addOne(rotStr)
										val axis = graphs(cell).midpoint
										val rot = new Quaternion(axis, 180f)
										orientation = orientation.mul(rot)

										instances.map(_.transform.idt().rotate(orientation))
										isDirty = true
									}
									updateColors
									0

								case _: Int => 0
							} 
							// println(materialMap(hit.faceId).get(ColorAttribute.Diffuse).asInstanceOf[ColorAttribute].color.toIntBits())
							val (cell, node) = parseMeshID(hit.faceId)
							val dir =  btn * (graphs(cell).isMirrored match {
								case true => -1
								case false => 1
							})
							

							if (dir != 0){
								if (Gdx.input.isKeyPressed(Keys.CONTROL_LEFT)){
									val rotStr = dir+"r"+cell+node
									state.Rotate(node.TwistFn(dir))
									state.moveList.addOne(rotStr)
									state.undoStack.clear()
									isDirty = true
									
										
									
								}else{
									val rotStr = dir+"t"+cell+node.toString()
									state.Twist(graphs(cell).color, node.TwistFn(dir)) 
									state.moveList.addOne(rotStr)
									state.undoStack.clear()
									isDirty = true
								}
								updateColors
								
							}
								
							

						case None =>
							// println("No hit")
							// dumpInstanceInfo()
							// updateFaceColorByPart("c20f12", new Color(1f, 0f, 0f, 1f))
					}
				}

				true
			}

			override def keyDown(keycode : Int): Boolean = {

				if (keycode >= Keys.F1 && keycode <= Keys.F12 && !Gdx.input.isKeyPressed(Keys.ALT_LEFT) && Gdx.input.isKeyPressed(Keys.ALT_RIGHT)) {
					val n = keycode - Keys.F1 + 1
					n match {
						case x if x <= 6 => 


							SwingUtilities.invokeLater(new Runnable {
								override def run(): Unit = {

									val confirm = JOptionPane.showConfirmDialog(null, "Are you sure you want to scramble?", "Scramble?", JOptionPane.YES_NO_OPTION)
									if (confirm == JOptionPane.YES_OPTION){
										state.ResetState

										if (x == 6){
											for (i <- 0 until 1000){
												randomMove
											}
										}else{
											for (i <- 0 until x){
												randomMove
											}
										}
										isDirty = true
									}
									Gdx.app.postRunnable(new Runnable {
										override def run(): Unit = {
											updateColors
										}
									})
									
								}
							})

						case 12 => 
							SwingUtilities.invokeLater(new Runnable {
								override def run(): Unit = {
									val confirm = JOptionPane.showConfirmDialog(null, "Are you sure you want to reset?", "Reset?", JOptionPane.YES_NO_OPTION)
									if (confirm == JOptionPane.YES_OPTION){
										state.ResetState
										isDirty = false
										
									
									}
									Gdx.app.postRunnable(new Runnable {
										override def run(): Unit = {
											updateColors
										}
									})
								}
							})


						// case 8 => 
						// 	println(state.moveList)
							
						
						case _ => ()
					}
					
				}
				if (keycode == Keys.TAB){
					show3rdLayer = !show3rdLayer
				}
				if (Gdx.input.isKeyPressed(Keys.CONTROL_LEFT)
					|| Gdx.input.isKeyPressed(Keys.CONTROL_RIGHT)
					|| Gdx.input.isKeyPressed(Keys.SYM)){
					keycode match {
						case Keys.S => state.runFileChooser(true)
						case Keys.O => state.runFileChooser(false)
						case Keys.Z =>
							if (Gdx.input.isKeyPressed(Keys.SHIFT_LEFT) 
								|| Gdx.input.isKeyPressed(Keys.SHIFT_RIGHT)){
								state.RedoMove
							}else{
								state.UndoMove
							}
							isDirty = true
						case Keys.Y => state.RedoMove
							isDirty = true
						case _ => ()
					}
				}
				true
			}

		}

		val modalInputBlocker: InputProcessor = new InputAdapter {

			

			override def keyDown(k: Int) = { Toolkit.getDefaultToolkit.beep(); true }
			override def keyUp(k: Int) = { Toolkit.getDefaultToolkit.beep(); true }
			override def keyTyped(c: Char) = { Toolkit.getDefaultToolkit.beep(); true }
			override def touchDown(x: Int, y: Int, p: Int, b: Int) = { Toolkit.getDefaultToolkit.beep(); true }
			override def touchUp(x: Int, y: Int, p: Int, b: Int) = { true }
			override def touchDragged(x: Int, y: Int, p: Int) = { Toolkit.getDefaultToolkit.beep(); true }

			
		}
	
}










