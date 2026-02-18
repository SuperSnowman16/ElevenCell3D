// file: ManualSplit3DExample.scala
import com.badlogic.gdx._
import com.badlogic.gdx.graphics._
import com.badlogic.gdx.graphics.g2d._
import com.badlogic.gdx.graphics.g3d._
import com.badlogic.gdx.graphics.g3d.environment._
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.utils.ModelBuilder
import com.badlogic.gdx.scenes.scene2d._
import com.badlogic.gdx.scenes.scene2d.ui._
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener
import com.badlogic.gdx.scenes.scene2d.utils.ChangeListener
import com.badlogic.gdx.scenes.scene2d.utils.TextureRegionDrawable
import com.badlogic.gdx.utils.viewport.ScreenViewport
import com.badlogic.gdx.math.Vector3
import com.badlogic.gdx.math.Vector2
import com.badlogic.gdx.graphics.g3d.utils.CameraInputController
import com.badlogic.gdx.scenes.scene2d.InputEvent
import com.badlogic.gdx.InputAdapter
import com.badlogic.gdx.scenes.scene2d.Actor
import com.badlogic.gdx.backends.lwjgl3.Lwjgl3ApplicationConfiguration
import com.badlogic.gdx.backends.lwjgl3.Lwjgl3Application

object ManualSplit3DExample {
  def main(args: Array[String]): Unit = {
    val cfg = new Lwjgl3ApplicationConfiguration()
    cfg.setTitle("Manual Split 3D Example (Scala, libGDX)")
    cfg.setWindowedMode(1200, 800)
    new Lwjgl3Application(new GameImpl, cfg)
  }

  class GameImpl extends ApplicationAdapter {
    // 3D
    private var modelBatch: ModelBatch = _
    private var model: Model = _
    private var instance: ModelInstance = _
    private var environment: Environment = _
    private var cam: PerspectiveCamera = _
    private var camController: CameraInputController = _

    // UI / Stage
    private var stage: Stage = _
    private var skin: Skin = _
    private var leftTable: Table = _
    private var rightTable: Table = _
    private var handle: Actor = _

    // Controls kept as fields
    private var autoRotate: CheckBox = _
    private var spinNow: TextButton = _
    private var scaleSlider: Slider = _

    // drawing helpers
    private var whiteTex: Texture = _

    // split control
    private var splitFraction = 0.68f // fraction of width given to left pane
    private val handleWidth = 10f

    override def create(): Unit = {
      // 3D setup
      modelBatch = new ModelBatch()
      val mb = new ModelBuilder()
      model = mb.createBox(2f, 2f, 2f,
        new Material(ColorAttribute.createDiffuse(Color.CYAN)),
        (com.badlogic.gdx.graphics.VertexAttributes.Usage.Position | com.badlogic.gdx.graphics.VertexAttributes.Usage.Normal).toLong)
      instance = new ModelInstance(model)
      instance.transform.setTranslation(0f, 0f, -6f)

      environment = new Environment()
      environment.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.6f, 0.6f, 0.6f, 1f))
      environment.add(new DirectionalLight().set(0.8f, 0.8f, 0.8f, -1f, -0.8f, -0.2f))

      cam = new PerspectiveCamera(67, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
      cam.position.set(0f, 1.5f, 4f)
      cam.lookAt(0f, 0f, -6f)
      cam.near = 0.1f
      cam.far = 100f
      cam.update()

      camController = new CameraInputController(cam)
      camController.translateUnits = 10f

      // Stage & skin
      stage = new Stage(new ScreenViewport())

      // tiny white texture and Drawable for styles
      val pix = new Pixmap(1, 1, Pixmap.Format.RGBA8888)
      pix.setColor(1f, 1f, 1f, 1f)
      pix.fill()
      whiteTex = new Texture(pix)
      pix.dispose()
      val whiteDrawable = new TextureRegionDrawable(new TextureRegion(whiteTex))

      skin = new Skin()
      val font = new BitmapFont()
      skin.add("default-font", font)

      // TextButton style
      val tbStyle = new TextButton.TextButtonStyle()
      tbStyle.up = whiteDrawable
      tbStyle.down = whiteDrawable
      tbStyle.checked = whiteDrawable
      tbStyle.font = font
      skin.add("default", tbStyle)

      // CheckBox style
      val cbStyle = new CheckBox.CheckBoxStyle()
      cbStyle.checkboxOn = whiteDrawable
      cbStyle.checkboxOff = whiteDrawable
      cbStyle.font = font
      skin.add("default", cbStyle)

      // Slider style (horizontal)
      val sliderStyle = new Slider.SliderStyle()
      sliderStyle.background = whiteDrawable
      sliderStyle.knob = whiteDrawable
      sliderStyle.knobBefore = whiteDrawable
      sliderStyle.knobAfter = whiteDrawable
      // register with the name Slider expects for horizontal
      skin.add("default-horizontal", sliderStyle)

      // Label style
      val labelStyle = new Label.LabelStyle(font, Color.WHITE)
      skin.add("default", labelStyle)

      // Prepare UI actors (we won't use SplitPane)
      leftTable = new Table()
      leftTable.setFillParent(false)
      // optional debug background
      // leftTable.setBackground(whiteDrawable) // uncomment to see the region

      rightTable = new Table()
      rightTable.setFillParent(false)
      rightTable.defaults().pad(8f)

      // create real controls and keep references
      autoRotate = new CheckBox(" Auto rotate", skin)
      autoRotate.setChecked(true)
      spinNow = new TextButton("Spin Now", skin)
      scaleSlider = new Slider(0.3f, 2f, 0.01f, false, skin)
      scaleSlider.setValue(1f)

      // assemble rightTable
      rightTable.add(new Label("Controls", skin)).row()
      rightTable.add(autoRotate).row()
      rightTable.add(spinNow).row()
      rightTable.add(new Label("Cube scale", skin)).row()
      rightTable.add(scaleSlider).row()

      // hook listeners
      spinNow.addListener(new ClickListener() {
        override def clicked(event: InputEvent, x: Float, y: Float): Unit = {
          instance.transform.rotate(Vector3.Y, 90f)
        }
      })
      scaleSlider.addListener(new ChangeListener() {
        override def changed(event: ChangeListener.ChangeEvent, actor: Actor): Unit = {
          val s = scaleSlider.getValue
          instance.transform.setToScaling(s, s, s)
          instance.transform.trn(0f, 0f, -6f)
        }
      })

      // handle actor (drag bar)
      handle = new Actor()
      handle.setSize(handleWidth, 10f)
      // small visible handle background
      handle.setColor(0.4f, 0.4f, 0.4f, 1f)
      handle.addListener(new InputListener {
        private var dragging = false
        override def touchDown(event: InputEvent, x: Float, y: Float, pointer: Int, button: Int): Boolean = {
          dragging = true
          true
        }
        override def touchDragged(event: InputEvent, x: Float, y: Float, pointer: Int): Unit = {
          if (dragging) {
            val screenX = event.getStageX // stage coords
            val stageWidth = stage.getViewport.getWorldWidth
            // compute new split fraction, clamp
            val frac = (screenX - handleWidth * 0.5f) / stageWidth
            splitFraction = Math.max(0.15f, Math.min(0.85f, frac))
          }
        }
        override def touchUp(event: InputEvent, x: Float, y: Float, pointer: Int, button: Int): Unit = {
          dragging = false
        }
      })

      // Add actors to stage (we will position/size them manually each frame)
      stage.addActor(leftTable)
      stage.addActor(handle)
      stage.addActor(rightTable)

      // Input handling: Stage first (UI), then camera wrapper that forwards only when pointer inside leftTable
      val multiplexer = new InputMultiplexer()
      val camWrapper = new InputAdapter {
        private def isInsideLeft(screenX: Int, screenY: Int): Boolean = {
          val sp = stage.screenToStageCoordinates(new Vector2(screenX.toFloat, screenY.toFloat))
          val lx = leftTable.getX
          val ly = leftTable.getY
          val lw = Math.max(1, leftTable.getWidth)
          val lh = Math.max(1, leftTable.getHeight)
          sp.x >= lx && sp.x <= lx + lw && sp.y >= ly && sp.y <= ly + lh
        }
        override def touchDown(screenX: Int, screenY: Int, pointer: Int, button: Int): Boolean = {
          if (isInsideLeft(screenX, screenY)) camController.touchDown(screenX, screenY, pointer, button)
          else false
        }
        override def touchDragged(screenX: Int, screenY: Int, pointer: Int): Boolean = {
          if (isInsideLeft(screenX, screenY)) camController.touchDragged(screenX, screenY, pointer)
          else false
        }
        override def touchUp(screenX: Int, screenY: Int, pointer: Int, button: Int): Boolean = {
          if (isInsideLeft(screenX, screenY)) camController.touchUp(screenX, screenY, pointer, button)
          else false
        }
        override def mouseMoved(screenX: Int, screenY: Int): Boolean = {
          if (isInsideLeft(screenX, screenY)) camController.mouseMoved(screenX, screenY)
          else false
        }
        override def scrolled(amountX: Float, amountY: Float): Boolean = {
          val sx = Gdx.input.getX
          val sy = Gdx.input.getY
          if (isInsideLeft(sx, sy)) camController.scrolled(amountX, amountY)
          else false
        }
      }

      multiplexer.addProcessor(stage)
      multiplexer.addProcessor(camWrapper)
      Gdx.input.setInputProcessor(multiplexer)

      Gdx.gl.glClearColor(0.12f, 0.12f, 0.14f, 1f)
    }

    override def render(): Unit = {
      val delta = Math.min(1 / 30f, Gdx.graphics.getDeltaTime)

      // update stage (actors) and camera controller (controller responds to forwarded events)
      stage.act(delta)

      // compute sizes based on splitFraction and viewport
      val fullW = stage.getViewport.getWorldWidth
      val fullH = stage.getViewport.getWorldHeight

      val leftW = Math.max(20f, fullW * splitFraction - handleWidth * 0.5f)
      val rightW = Math.max(120f, fullW - leftW - handleWidth)

      // place leftTable at (0,0)
      leftTable.setBounds(0f, 0f, leftW, fullH)

      // place handle next to leftTable
      handle.setBounds(leftW, 0f, handleWidth, fullH)

      // place rightTable to the right of handle
      rightTable.setBounds(leftW + handleWidth, 0f, rightW, fullH)

      // Render 3D into leftTable rectangle (stage coords match GL viewport coords)
      val leftX = leftTable.getX.toInt
      val leftY = leftTable.getY.toInt
      val leftH = Math.max(1, leftTable.getHeight.toInt)
      val leftWi = Math.max(1, leftTable.getWidth.toInt)

      if (leftWi > 0 && leftH > 0) {
        Gdx.gl.glEnable(GL20.GL_SCISSOR_TEST)
        Gdx.gl.glScissor(leftX, leftY, leftWi, leftH)
        Gdx.gl.glViewport(leftX, leftY, leftWi, leftH)
        Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT)

        cam.viewportWidth = leftWi.toFloat
        cam.viewportHeight = leftH.toFloat
        cam.update()

        if (autoRotate.isChecked) instance.transform.rotate(Vector3.Y, 20f * Gdx.graphics.getDeltaTime)

        modelBatch.begin(cam)
        modelBatch.render(instance, environment)
        modelBatch.end()

        Gdx.gl.glDisable(GL20.GL_SCISSOR_TEST)
        Gdx.gl.glViewport(0, 0, Gdx.graphics.getWidth, Gdx.graphics.getHeight)
      }

      // draw UI on top
      stage.draw()
    }

    override def resize(width: Int, height: Int): Unit = {
      stage.getViewport.update(width, height, true)
    }

    override def dispose(): Unit = {
      modelBatch.dispose()
      model.dispose()
      stage.dispose()
      skin.dispose()
      if (whiteTex != null) whiteTex.dispose()
    }
  }
}
