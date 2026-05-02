import scala.collection.mutable.HashMap

import Graphs.Graph
import scala.util.Random
import scala.collection.mutable.ListBuffer
import com.badlogic.gdx.Gdx
import javax.swing.JFileChooser
import javax.swing.SwingUtilities
import java.io.File
import scala.collection.mutable.Stack
import javax.swing.JOptionPane
import Permutations.Permutation
import Hyperboloid.IdMatrix

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
        main.hTransform = IdMatrix
        main.permTransform = Permutations.Identity(11)

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
                
                // wings and petals
                for (i <- 0 until 5){
                    stateMap.addOne((color, (0 until 3).map(j => vert.getFace(i+j).oppCell).appended(color).toSet), color)
                    stateMap.addOne((color, (0 until 4).map(j => vert.getFace(i+j).oppCell).appended(color).toSet), color)
                }
                
            }
        }

        println(stateMap.size)

        main.markColorsDirty()

        // println(stateMap.mkString("\n"))
    }

    

    

    def Twist(cell:Int, gripFn:Int=>Int){
        this.synchronized {
            // println("twisted cell:" + cell)
            val affectedPieces = stateMap.toArray.filter(kv => kv._1._2.contains(cell))
            for ((piece, color) <- affectedPieces){
                val oldCell = piece._1
                val grip = piece._2
                val newCell = gripFn(oldCell)
                // println(f + "+" + grip)
                // println(grip.toArray.map(gripMap.get(face,_).mkString(",")).mkString(";"))
                val newGrip = grip.map(gripFn)
                // println(f + " + " + grip + " -> " + newF + " + " + newGrip + " + " + color)
                stateMap.addOne((newCell,newGrip),color)
            }
            main.markColorsDirty()
            // undoStack.clear()
        }
    }

    def Rotate(gripFn:Int=>Int){
        this.synchronized {
            // println("twisted cell:" + cell)
            val affectedPieces = stateMap.toArray
            for ((piece, color) <- affectedPieces){
                val oldCell = piece._1
                val grip = piece._2
                val newCell = gripFn(oldCell)
                // println(f + "+" + grip)
                // println(grip.toArray.map(gripMap.get(face,_).mkString(",")).mkString(";"))
                val newGrip = grip.map(gripFn)
                // println(f + " + " + grip + " -> " + newF + " + " + newGrip + " + " + color)
                stateMap.addOne((newCell,newGrip),color)
            }
            main.markColorsDirty()
        }
    }

    def isSolved : Boolean = {
        val colorLocations = (0 to 10).map(x => stateMap.get((x, Set(x))).get).toArray
        for ((k,v) <- stateMap){
            if (k._1 != colorLocations(v)){
                // println("" + k + " " + v + " " + colorLocations(v))
            }
        }
        stateMap.map(x => colorLocations(x._1._1) == x._2).reduce(_ && _)
    }

    def Randomise {
        for ((k,v) <- stateMap){
            stateMap.addOne(k, Random.nextInt(11))
        }
    }


    val charArr11 = Array('0','1','2','3','4','5','6','7','8','9','t')

    def getOrientation : String = {
        val colorLocations = (0 to 10).map(x => stateMap.get((x, Set(x))).get).toArray
        colorLocations.map(x => charArr11(x)).mkString("")
    }

    

    



    def get(key:(Int, Set[Int])) : Int = stateMap.get(key).getOrElse(11)

    def runFileChooser(saveMode: Boolean): Unit = {
        SwingUtilities.invokeLater(new Runnable {
            override def run(): Unit = {
                val cwd = new java.io.File(System.getProperty("user.dir") + "/Saves")
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
            case s"${perm}:${dir}t${twist}" => 
                val newPerm = new Permutation(perm.map(c => charArr11.indexOf(c)).toArray)
                val offsetPerm = newPerm*main.permTransform.inv
                val (cell, node) = main.parseMeshID("c"+twist)
                println("twist")
                Rotate(x => offsetPerm(x))
                Twist(main.graphs(cell).color, node.TwistFn(dir.toInt))
                Rotate(x => offsetPerm.inv(x))

            case s"${dir}t${twist}" => 
                val (cell, node) = main.parseMeshID("c"+twist)
        
                Twist(main.graphs(cell).color, node.TwistFn(dir.toInt))

            // case s"${dir}r${rotation}" => 
            //     val (cell, node) = main.parseMeshID("c"+rotation)
            //     Rotate(node.TwistFn(dir.toInt))

            // case s"c${centerCell}" => 
            //     var cell = centerCell.toInt
            //     Rotate(main.CenterCell(cell))

            case _ => 
                println("invalid string ("+s+")")
                throw new Exception()

        }
        
    } 

    def StrUndo(s:String){
        println(s)
        undoStack.push(s)
        s match {
            case s"${perm}:${dir}t${twist}" => 
                val newPerm = new Permutation(perm.map(c => charArr11.indexOf(c)).toArray)
                val offsetPerm = newPerm*main.permTransform.inv
                val (cell, node) = main.parseMeshID("c"+twist)
                println("twist")
                Rotate(x => offsetPerm(x))
                Twist(main.graphs(cell).color, node.TwistFn(-dir.toInt))
                Rotate(x => offsetPerm.inv(x))


            // case s"${dir}r${rotation}" => 
            //     val (cell, node) = main.parseMeshID("c"+rotation)
            //     println("rotate")
            //     Rotate(node.TwistFn(-dir.toInt))

            // case s"c${centerCell}" => 
            //     println("center")
            //     Rotate(main.CenterCell(centerCell.toInt))

            case _ => 
                println("invalid string ("+s+")")
                throw new Exception()
        }
    }
    

    def saveState(path: String): Unit = {
        val handle = Gdx.files.absolute(path)
        handle.writeString(scrambleList.mkString(",")+";"+moveList.mkString(","), false)
        println(s"Saved to $path")
        Main.isDirty = false
    }

    def oldCellConverter(cellStr:String) : Int = {
        val cell = cellStr.toInt
        println("input" + cell)
        if (cell == 20){
            println("output" + 0)
            return 0
        }
        if (cell < 20){
            println("output" + (cell + 1))
            return (cell + 1)
        }
        println("output" + cell)
        return cell
    }

    def oldMoveParser(move:String) : String = {
        move match {
            case s"c${centerCell}" => 
                return s"c${oldCellConverter(centerCell)}"

            case _: String => 
                
                val nums = move.split("[tcmfev]")
                println(nums.mkString(","))
                val letters = move.filter(_.isLetter)
                nums(1) = oldCellConverter(nums(1)).toString()
                val s = nums(0) + letters(0) + nums(1) + letters(1) + nums(2)
                return s
        }
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
                                // StrToMove(oldMoveParser(move))
                                // scrambleList.addOne(oldMoveParser(move))
                            }
                        }
                        if (moves.nonEmpty){
                            for (move <- moves.split(",")){
                                StrToMove(move)
                                moveList.addOne(move)
                                // StrToMove(oldMoveParser(move))
                                // moveList.addOne(oldMoveParser(move))
                            }
                        }
                        println("Scramble: " + scrambleList.length + " moves")
                        println("Twists: " + moveList.length + " moves")
                        Main.isDirty = false
                        

                        
                    case _: String => 
                        println("invalid file")
                        new Exception("invalid file loaded")
                }

                Gdx.app.postRunnable(new Runnable {
                    override def run(): Unit = {
                        // main.updateColors
                        // println(stateMap.mkString("\n"))
                        print(isSolved)
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
                // main.updateColors   
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
            // main.updateColors
        }
        // println("redo:")
        // println(moveList)
        // println(undoStack)
    }


  
}
