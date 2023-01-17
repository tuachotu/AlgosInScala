package algo


/*
1,1,2,3

1 -2  { 1 + 3
2 -1
3 -1
 */
object MinimumIncrementToMakeArrayUnique extends App{


  def minIncrementForUnique2(nums: Array[Int]): Int = {
    import scala.collection.mutable.HashSet
    nums.indices.foldLeft((HashSet.empty[Int], 0)) { case ((lookupSet, incrementCounter), numberIndex) =>
      if (lookupSet.contains(nums(numberIndex))) {
        var i = 0
        while (lookupSet.contains(nums(numberIndex) + i)) {
          i = i + 1
        }
        nums(numberIndex) = nums(numberIndex) + i
        (lookupSet += nums(numberIndex), incrementCounter + i)
      } else {
        (lookupSet += nums(numberIndex), incrementCounter)
      }
    }._2
  }

//  println(minIncrementForUnique2(Array(1,2,2)))
//
//  println(minIncrementForUnique2(Array(3,2,1,2,1,7)))
  println(minIncrementForUnique2(Array(1,1,1,1,1)))

  /*
  (1,1,1,1,1)
  (1,2,1,1,1) -> 1
  (1,2,3,1,1) -> 3
  (1,2,3,4,1) -> 6
  (1,2,3,4,5) -> 10
   */

}
