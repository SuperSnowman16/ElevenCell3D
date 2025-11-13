import scala.collection.mutable.HashMap

import Graphs.Graph
import scala.util.Random
import scala.collection.mutable.ListBuffer
import com.badlogic.gdx.Gdx
import javax.swing.JFileChooser
import javax.swing.SwingUtilities
import java.io.File
import scala.collection.mutable.Stack

class State(cells:Array[Graph], main:Main) {

    val stateMap = new HashMap[(Int, Set[Int]), Int]


    val scrambleList = new ListBuffer[String]

    val moveList = new ListBuffer[String]

    val undoStack = new Stack[String]

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

    

    

    def Twist(cell:Int, gripFn:Int=>Int){
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
        // undoStack.clear()
        
        // main.updateColors
    }

    def Rotate(gripFn:Int=>Int){
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
        
        main.updateColors
    }

    def Randomise {
        for ((k,v) <- stateMap){
            stateMap.addOne(k, Random.nextInt(11))
        }
    }

    def get(key:(Int, Set[Int])) : Int = stateMap.get(key).getOrElse(11)

    def runFileChooser(saveMode: Boolean): Unit = {
        SwingUtilities.invokeLater(new Runnable {
            override def run(): Unit = {
                val cwd = new java.io.File(System.getProperty("user.dir"))
                val chooser = new JFileChooser(cwd)
                val result = if (saveMode) chooser.showSaveDialog(null)
                    else chooser.showOpenDialog(null)

                if (result == JFileChooser.APPROVE_OPTION) {
                    val file: File = chooser.getSelectedFile
                    
                    if (saveMode) { 
                        Gdx.app.postRunnable(new Runnable {
                            override def run(): Unit = {
                                saveState(file.getAbsolutePath)
                            }
                        })
                    } else {
                        Gdx.app.postRunnable(new Runnable {
                            override def run(): Unit = {
                                loadState(file.getAbsolutePath)
                            }
                        })
                    }
                    
                }
            }
            
            
        })
    }

    def StrToMove(s:String) {
        s match {
            case s"${dir}t${twist}" => 
                val (cell, node) = main.parseMeshID("c"+twist)
                Twist(main.graphs(cell).color, node.TwistFn(dir.toInt))

            case s"${dir}r${rotation}" => 
                val (cell, node) = main.parseMeshID("c"+rotation)
                Rotate(node.TwistFn(dir.toInt))

            case s"c${centerCell}" => 
                Rotate(main.CenterCell(centerCell.toInt))

            case _ => 
                println("invalid string ("+s+")")
                throw new Exception()

        }
        
    } 

    def StrUndo(s:String){
        undoStack.push(s)
        s match {
            case s"${dir}t${twist}" => 
                val (cell, node) = main.parseMeshID("c"+twist)
                Twist(main.graphs(cell).color, node.TwistFn(-dir.toInt))

            case s"${dir}r${rotation}" => 
                val (cell, node) = main.parseMeshID("c"+rotation)
                Rotate(node.TwistFn(-dir.toInt))

            case s"c${centerCell}" => 
                Rotate(main.CenterCell(centerCell.toInt))

            case _ => 
                println("invalid string ("+s+")")
                throw new Exception()
        }
    }
    

    


    def saveState(path: String): Unit = {
        val handle = Gdx.files.absolute(path)
        handle.writeString(scrambleList.mkString(",")+";"+moveList.mkString(","), false)
        println(s"Saved to $path")
    }

    def loadState(path: String): Unit = {
        
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
                                StrToMove(move) 
                                scrambleList.addOne(move) 
                            }
                        }
                        if (moves.nonEmpty){
                            for (move <- moves.split(",")){
                                StrToMove(move)
                                moveList.addOne(move)
                            }
                        }
                        
                    case _: String => 
                        println("invalid file")
                        new Exception("invalid file loaded")
                }

                Gdx.app.postRunnable(new Runnable {
                    override def run(): Unit = {
                        main.updateColors
                    }
                })

                

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

    def UndoMove {
        moveList.lastOption match {
            case None => return
            case Some(str) => 
                moveList.dropRightInPlace(1)
                StrUndo(str) 
                main.updateColors   
        }
        // println("undo:")
        // println(moveList)
        // println(undoStack)
    }

    def RedoMove {
        if (undoStack.nonEmpty){
            val str = undoStack.pop()
            StrToMove(str)
            moveList.addOne(str)
            main.updateColors
        }
        // println("redo:")
        // println(moveList)
        // println(undoStack)
    }


  
}
