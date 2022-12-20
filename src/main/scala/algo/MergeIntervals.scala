package algo

import scala.collection.SeqView.Sorted



// Solution: Traverse the list and as we got check the end an start of adjacsent intervals
// // foldLest will take an empty list of interval and which needs to be built as we go

object MergeIntervals extends App {
 def merge(intervals: List[(Int, Int)]): List[(Int, Int)] = {
   intervals.tail.foldLeft(List[(Int,Int)](intervals.head)) { (result, interval) =>
     val boundaryInteval = result.last
     val newBoundaryInterval = if(boundaryInteval._2 >= interval._1) List((boundaryInteval._1, interval._2)) else List(boundaryInteval, interval)

     if(result.length == 1) {
       newBoundaryInterval
     } else {
       result.init ++ newBoundaryInterval
     }

   }
 }
  case class Interval(start: Int, end: Int) extends Ordered[Interval] {
    override def compare(that: Interval): Int = this.start compareTo that.start
  }


  def mergeRandomOrder(intervals:List[Interval]): List[Interval] = {
    val sortedInterval = intervals.sorted.toList
    sortedInterval.tail.foldLeft(List[Interval](sortedInterval.head)) { (result, interval) =>
      val boundaryInteval = result.last
      val newBoundaryInterval = if (boundaryInteval.end >= interval.start) List(Interval(boundaryInteval.start, interval.end)) else List(boundaryInteval, interval)

      if (result.length == 1) {
        newBoundaryInterval
      } else {
        result.init ++ newBoundaryInterval
      }

    }
  }

//  println(merge(List((1,3),(2,6),(8,10),(15,18))).mkString(","))
//  println(merge(List((1,3),(8,10),(2,6),(15,18))).mkString(","))
//  println(merge(List((1,4),(4,5))).mkString(","))


  println(mergeRandomOrder(List(Interval(1,3),Interval(8,10),Interval(2,6),Interval(15,18))).mkString(","))

}
