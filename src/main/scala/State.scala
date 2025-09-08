import scala.collection.mutable.HashMap

import Graphs.Graph
import scala.util.Random
class State(cells:Array[Graph]) {

    val stateMap = new HashMap[(Int, Set[Int]), Int]

    ResetState
    Randomise
    

    def ResetState {
        stateMap.clear()

        for (cell <- cells){
            val color = cell.color
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

    def Randomise {
        for ((k,v) <- stateMap){
            stateMap.addOne(k, Random.nextInt(11))
        }
    }

    def get(key:(Int, Set[Int])) : Int = stateMap.get(key).getOrElse(11)


  
}
