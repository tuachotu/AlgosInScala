package algo

// N+1 numbers from 1 to N
// input : List [1,2,3,3] - 3
//
object FindDuplicate extends App{
  def findDuplicateV1(l:List[Int]): Unit = {
    val lookupMap = l.foldLeft(Map[Int,Int]()) { case (lookup, number) =>
      lookup + (number -> (lookup.getOrElse(number, 0) + 1))
    }
    val k = (lookupMap.filter {case (k,v) => v > 1}).keys.toList.head
    println(k)
  }

  def findDuplicateV2(l: List[Int]): Unit = { // works well with one duplicate
    println(l.sum - l.indices.sum)
  }

  def findDuplicateV3(l: List[Int]): Unit = {
    val sortedList = l.sorted
    val res = sortedList.indices.takeWhile { index =>
      if (index == 0) true else sortedList(index) != sortedList(index-1)
    }.toList.last
    println(sortedList(res))
  }


  findDuplicateV3(List(1,2,3,3))
  findDuplicateV3(List(2,2,3,3))
}
