import scala.collection.mutable.HashMap

import Graphs.Graph
import scala.util.Random
class State(cells:Array[Graph], main:MyGame) {

    val stateMap = new HashMap[(Int, Set[Int]), Int]

    ResetState
    // Randomise
    

    def ResetState {
        stateMap.clear()

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

        println(stateMap.mkString("\n"))
    }

    def Twist(cell:Int, gripFn:Int=>Int){
        println("twisted cell:" + cell)
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
        main.updateColors
    }

    def Randomise {
        for ((k,v) <- stateMap){
            stateMap.addOne(k, Random.nextInt(11))
        }
    }

    def get(key:(Int, Set[Int])) : Int = stateMap.get(key).getOrElse(11)


  
}
