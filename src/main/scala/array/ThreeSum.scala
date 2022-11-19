package array

import scala.collection.mutable.HashMap

object ThreeSum extends  App{
  def twoSumReturnNumbers(list: List[Int], target: Int): List[(Int, Int)] = {
    val visited = HashMap[Int, Int]() // Value -> Index

    list.zipWithIndex.foldLeft(List[(Int,Int)]()) { case (result, (number, index)) =>
      val lookup = target - number
      visited(number) = index
      if (visited.contains(lookup))  (lookup, number)::result else result
    }
  }

  def threeSumPrintNumber(list:List[Int], target: Int): Unit = {
    list.zipWithIndex foreach { case (number, index) =>
      val targetTwoSum = target - number
      val restList = list.take(index-1) ++ list.drop(index)
      twoSumReturnNumbers(restList, targetTwoSum) match {
        case Nil =>
        case resultTwoSum => resultTwoSum foreach { pair =>
          println(number, pair._1, pair._2)
        }
      }


    }
  }
  threeSumPrintNumber(List(7, 12, 3, 1, 2, -6, 5, -8, 6), 0)


}
