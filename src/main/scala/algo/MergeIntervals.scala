package algo

object MergeIntervals extends App {
 def merge(intervals: List[(Int, Int)]): List[(Int, Int)] = {
   intervals.foldLeft(List[(Int,Int)](intervals.head)) { (result, interval) =>
     val boundaryInteval = result.last
     val newBoundaryInterval = if(boundaryInteval._2 >= interval._1) List((boundaryInteval._1, interval._2)) else List(boundaryInteval, interval)

     if(result.length == 1) {
       newBoundaryInterval
     } else {
       result.init ++ newBoundaryInterval
     }

   }
 }

  println(merge(List((1,3),(2,6),(8,10),(15,18))).mkString(","))
  println(merge(List((1,4),(4,5))).mkString(","))

}
