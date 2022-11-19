package array

import collection.mutable.HashMap
object TwoSum extends App{
  def twoSumPrintNumbers(list: List[Int], target: Int): Unit = {
    val visited = HashMap[Int, Int]() // Value -> Index

    list.zipWithIndex foreach { case (number, index) =>
      val lookup = target - number
      visited(number) = index
      if (visited.contains(lookup)) println(lookup, number)
    }
  }

  def twoSumReturnNumbers(list: List[Int], target: Int): List[(Int, Int)] = {
    val visited = HashMap[Int, Int]() // Value -> Index

    list.zipWithIndex.foldLeft(List[(Int,Int)]()) { case (result, (number, index)) =>
      val lookup = target - number
      visited(number) = index
      if (visited.contains(lookup))  (lookup, number)::result else result
    }
  }


  twoSumPrintNumbers(List(9,3,4,5,6,7,8, 8), 16)
  println
  print(twoSumReturnNumbers(List(9,3,4,5,6,7,8, 8), 16))

}
