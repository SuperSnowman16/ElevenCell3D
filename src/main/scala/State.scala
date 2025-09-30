import scala.collection.mutable.HashMap

import Graphs.Graph
import scala.util.Random
import scala.collection.mutable.ListBuffer
import com.badlogic.gdx.Gdx
import javax.swing.JFileChooser
import javax.swing.SwingUtilities
import java.io.File

class State(cells:Array[Graph], main:Main) {

    val stateMap = new HashMap[(Int, Set[Int]), Int]


    val scrambleList = new ListBuffer[String]

    val moveList = new ListBuffer[String]

    ResetState
    // Randomise
    

    def ResetState {
        stateMap.clear()
        moveList.clear()
        scrambleList.clear()

        for (cell <- cells){
            val color = cell.color
            stateMap.addOne((color, Set(color)), color)
            for (face <- cell.faces){
                stateMap.addOne((color, Set(color, face.oppCell)), color)
            }
            for (edge <- cell.edges){
                stateMap.addOne((color, edge.faces.map(f => f.oppCell).appended(color).toSet), color)
            }
            for (vert <- cell.verts){
                stateMap.addOne((color, vert.faces.map(f => f.oppCell).appended(color).toSet), color)
            }
        }

        // main.updateColors

        // println(stateMap.mkString("\n"))
    }

    

    def Twist(cell:Int, gripFn:Int=>Int, twistStr:String, isScramble:Boolean = false){
        // println("twisted cell:" + cell)
        val affectedPieces = stateMap.toArray.filter(kv => kv._1._2.contains(cell))
        for ((piece, color) <- affectedPieces){
            var oldCell = piece._1
            val grip = piece._2
            val newCell = gripFn(oldCell)
            // println(f + "+" + grip)
            // println(grip.toArray.map(gripMap.get(face,_).mkString(",")).mkString(";"))
            val newGrip = grip.map(gripFn)
            // println(f + " + " + grip + " -> " + newF + " + " + newGrip + " + " + color)
            
            stateMap.addOne((newCell,newGrip),color)
        }
        if (isScramble){
            scrambleList.addOne(twistStr)
        }else{
            moveList.addOne(twistStr)
        }
        // main.updateColors
    }

    def Rotate(gripFn:Int=>Int, rotStr:String){
        // println("twisted cell:" + cell)
        val affectedPieces = stateMap.toArray
        for ((piece, color) <- affectedPieces){
            var oldCell = piece._1
            val grip = piece._2
            val newCell = gripFn(oldCell)
            // println(f + "+" + grip)
            // println(grip.toArray.map(gripMap.get(face,_).mkString(",")).mkString(";"))
            val newGrip = grip.map(gripFn)
            // println(f + " + " + grip + " -> " + newF + " + " + newGrip + " + " + color)
            
            stateMap.addOne((newCell,newGrip),color)
        }
        moveList.addOne(rotStr)
        main.updateColors
    }

    def Randomise {
        for ((k,v) <- stateMap){
            stateMap.addOne(k, Random.nextInt(11))
        }
    }

    def get(key:(Int, Set[Int])) : Int = stateMap.get(key).getOrElse(11)

    def runFileChooser(saveMode: Boolean): Unit = {
        SwingUtilities.invokeLater(() => {
            val cwd = new java.io.File(System.getProperty("user.dir"))
            val chooser = new JFileChooser(cwd)
            val result = if (saveMode) chooser.showSaveDialog(null)
                else chooser.showOpenDialog(null)

            if (result == JFileChooser.APPROVE_OPTION) {
            val file: File = chooser.getSelectedFile
            Gdx.app.postRunnable(new Runnable {
                override def run(): Unit = {
                if (saveMode) {
                    saveGame(file.getAbsolutePath)
                } else {
                    loadGame(file.getAbsolutePath)
                }
                }
            })
            }
        })
    }

    def StrToMove(s:String, isScramble:Boolean = false) {
        val graphs = main.graphs
        s match {
            case s"${dir}t${twist}" => 
                val (cell, node) = main.parseMeshID("c"+twist)
                println(cell, node)
                println(graphs(cell).color)
                println(dir.toInt)
                Twist(graphs(cell).color, node.TwistFn(dir.toInt), s, isScramble)

            case s"${dir}r${rotation}" => 
                val (cell, node) = main.parseMeshID("c"+rotation)
                Rotate(node.TwistFn(dir.toInt), s)

            case s"c${centerCell}" => 
                Rotate(main.CenterCell(centerCell.toInt), s)

            case _ => 
                println("invalid string ("+s+")")
                throw new Exception()

        }
        
    } 

    


    def saveGame(path: String): Unit = {
        val handle = Gdx.files.absolute(path)
        handle.writeString(scrambleList.mkString(",")+";"+moveList.mkString(","), false)
        println(s"Saved to $path")
    }

    def loadGame(path: String): Unit = {
        val handle = Gdx.files.absolute(path)
        if (handle.exists()) {
            try {
                ResetState
                val data = handle.readString() match {
                    case s"${scramble};${moves}" =>
                        // println(scramble)
                        // println(moves)
                        if (scramble.nonEmpty){
                            for (move <- scramble.split(",")){
                                StrToMove(move, true)
                            }
                        }
                        if (moves.nonEmpty){
                            for (move <- moves.split(",")){
                                StrToMove(move)
                            }
                        }
                        
                    case _: String => 
                        println("invalid file")
                        new Exception("invalid file loaded")
                }

                main.updateColors

                

                // println("Loaded1")

            } catch {
                case _ : Throwable => 
                    println("load error")
                    throw new Exception()
                    ResetState

            }
            
            // println("Loaded")
        }
    }


  
}
