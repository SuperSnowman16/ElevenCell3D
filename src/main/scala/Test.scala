import com.badlogic.gdx._
import com.badlogic.gdx.graphics._
import com.badlogic.gdx.graphics.g3d._
import com.badlogic.gdx.graphics.g3d.attributes._
import com.badlogic.gdx.graphics.g3d.environment._
import com.badlogic.gdx.graphics.g3d.utils._
import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.math.Vector3
import scala.collection.mutable

class Test extends ApplicationAdapter {
  var modelBatch: ModelBatch = _
  var environment: Environment = _
  var camera: PerspectiveCamera = _
  var model: Model = _
  var instance: ModelInstance = _

  // store faceId -> material
  val materialMap = mutable.Map[String, Material]()

  override def create(): Unit = {
    modelBatch = new ModelBatch()

    environment = new Environment()
    environment.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.8f, 0.8f, 0.8f, 1f))
    environment.add(new DirectionalLight().set(1f, 1f, 1f, -1f, -0.8f, -0.2f))

    camera = new PerspectiveCamera(67, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    camera.position.set(3f, 3f, 3f)
    camera.lookAt(0f, 0f, 0f)
    camera.near = 0.1f
    camera.far = 100f
    camera.update()

    val builder = new ModelBuilder()
    builder.begin()
    val attrs = Usage.Position | Usage.Normal

    def makeFace(id: String, v1: Vector3, v2: Vector3, v3: Vector3, v4: Vector3, normal: Vector3, color: Color): Unit = {
      val mat = new Material(
        ColorAttribute.createDiffuse(color),
        new BlendingAttribute(GL20.GL_SRC_ALPHA, GL20.GL_ONE_MINUS_SRC_ALPHA)
      )
      val part = builder.part(id, GL20.GL_TRIANGLES, attrs, mat)
      part.rect(v1, v2, v3, v4, normal)
      materialMap += id -> mat
    }

    // cube vertices
    val p000 = new Vector3(-1, -1, -1)
    val p001 = new Vector3(-1, -1,  1)
    val p010 = new Vector3(-1,  1, -1)
    val p011 = new Vector3(-1,  1,  1)
    val p100 = new Vector3( 1, -1, -1)
    val p101 = new Vector3( 1, -1,  1)
    val p110 = new Vector3( 1,  1, -1)
    val p111 = new Vector3( 1,  1,  1)

    // 6 faces with correct normals
    makeFace("front",  p101, p001, p011, p111, Vector3.Z,        new Color(1,0,0,0.6f))
    makeFace("back",   p100, p110, p010, p000, Vector3.Z.scl(-1),new Color(0,1,0,0.6f))
    makeFace("left",   p001, p000, p010, p011, Vector3.X.scl(-1),new Color(0,0,1,0.6f))
    makeFace("right",  p100, p101, p111, p110, Vector3.X,        new Color(1,1,0,0.6f))
    makeFace("top",    p110, p111, p011, p010, Vector3.Y,        new Color(0,1,1,0.6f))
    makeFace("bottom", p100, p000, p001, p101, Vector3.Y.scl(-1),new Color(1,0,1,0.6f))

    model = builder.end()
    instance = new ModelInstance(model)

    // simple click handler to test updating a face
    Gdx.input.setInputProcessor(new InputAdapter {
      override def touchDown(x: Int, y: Int, pointer: Int, button: Int): Boolean = {
        // for now just toggle the "front" face color
        updateFaceColor("front", new Color(0f, 1f, 0f, 0.6f))
        true
      }
    })
  }

  override def render(): Unit = {
    Gdx.gl.glViewport(0, 0, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)

    modelBatch.begin(camera)
    modelBatch.render(instance, environment)
    modelBatch.end()
  }

  override def dispose(): Unit = {
    modelBatch.dispose()
    model.dispose()
  }

  // 🔹 Mutate the diffuse attribute only
  def updateFaceColor(faceId: String, newColor: Color): Unit = {
    materialMap.get(faceId).foreach { mat =>
      val diffuse = mat.get(ColorAttribute.Diffuse).asInstanceOf[ColorAttribute]
      diffuse.color.set(newColor)
    }
  }
}
