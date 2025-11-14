import scala.collection.mutable.ListBuffer


object Permutations {

    def TuplePermutation(arr:Array[(Int,Int)]) : Permutation = {
        val newArr = Array.range(0,57)
        for ((x,y) <- arr){
            val (a,b) = (x-1, y-1)
            newArr(a)=b
            newArr(b)=a
        }
        return new Permutation(newArr)
    }

    def Identity : Permutation = {
        new Permutation(Array.range(0,57))
    }

    val m1 = TuplePermutation(Array(( 3,52),( 4,37),( 5,44),( 6,38),( 7,32),( 8,30),( 9,33),(10,16),(11,28),(12,49),(13,47),(14,54),(15,36),(17,34),(18,31),(19,50),(21,39),(22,46),(23,25),(24,40),(26,41),(27,56),(29,43),(35,45),(42,51),(48,55)))

    val m2 = TuplePermutation(Array(( 2,19),( 3,53),( 4,14),( 5,10),( 6,35),( 7,28),( 8,52),( 9,51),(11,56),(12,54),(13,26),(15,17),(16,43),(18,32),(22,27),(23,57),(25,39),(29,47),(31,46),(33,55),(34,37),(36,49),(38,42),(40,50),(41,44),(45,48)))

    val m3 = TuplePermutation(Array(( 2,57),( 4,56),( 5,45),( 6,38),( 7,46),( 8,30),( 9,25),(10,40),(11,36),(12,54),(13,39),(14,49),(15,28),(16,24),(17,31),(18,34),(19,55),(20,53),(21,47),(22,32),(23,33),(26,51),(27,37),(35,44),(41,42),(48,50)))

    val m4 = TuplePermutation(Array(( 1,20),( 2,53),( 3,19),( 4,16),( 5,34),( 6,38),( 7,31),( 8,40),( 9,48),(10,37),(11,22),(12,47),(13,49),(14,43),(15,41),(17,44),(18,32),(24,30),(26,36),(27,56),(28,46),(29,54),(33,55),(35,42),(45,51),(50,52)))

    val f0 = m1*m2

    val e0 = m1*m3

    val v0 = m2*m3
    

    val fp = (m3*m1*m2).fixedPts

    class Permutation(val arr:Array[Int]) {
        def size = arr.size
        def apply(x:Int) : Int = arr(x)

        def *(p:Permutation):Permutation = {
            require(size == p.size)
            val newArr = new Array[Int](size)
            for(i <- 0 until size){
                newArr(i) = arr(p.arr(i))
            }
            return new Permutation(newArr)
        }

        def inv : Permutation = {
            val newArr = new Array[Int](size)
            for(i <- 0 until size){
                newArr(arr(i)) = i
            }
            return new Permutation(newArr)
        }

        def ^(n:Int) : Permutation = n match {
            case x if x > 0 => Array.fill(n)(new Permutation(arr)).reduce(_*_)
            case x if x == 0 => Permutations.Identity
            case x if x < 0 => Array.fill(-n)(new Permutation(inv.arr)).reduce(_*_)
        }
        

        override def toString(): String = {
            arr.zipWithIndex.toString()
        }

        override def equals(obj : Any): Boolean = {
            obj match {
                case p: Permutation => 
                    arr.sameElements(p.arr)
                case _ : Object => false
            }
        }



        def fixedPts : ListBuffer[Int] = {
            val list = new ListBuffer[Int]
            for(i <- 0 until size){
                if (arr(i)==i){
                    list += i
                }
            }
            return list
        }
    }

    


}



