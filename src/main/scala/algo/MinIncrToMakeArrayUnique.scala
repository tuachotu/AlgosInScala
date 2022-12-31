package algo

/*
Example 1:
Input: nums = [1,2,2]
Output: 1
Explanation: After 1 move, the array could be [1, 2, 3].
1 - 0
2 - 1
3 - 2   1


Example 2:
Input: nums = [3,2,1,2,1,7]
Output: 6
Explanation: After 6 moves, the array could be [3, 4, 1, 2, 5, 7].
It can be shown with 5 or less moves that it is impossible for the array to have all unique values.


3 - 0
2 - 1
1 - 2
4  - 3
5 - 4

2 ? exists yes , +1 = 3  exists yes +1 = 4 exists not, (count - 2 )
2 ? exists yes , +1 = 3  exists yes +1 = 4 exists yes +1 = 5 exists no ( count 2 +3)
1

 */
import scala.collection.mutable.HashSet
object MinIncrToMakeArrayUnique extends App {
  def minIncrementForUnique(nums: Array[Int]): Int = {
    nums.indices.foldLeft((HashSet.empty[Int], 0))  { case ((lookupSet, incrementCounter), numberIndex) =>
      if (lookupSet.contains(nums(numberIndex))) {
        var i  = 0
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


  println(minIncrementForUnique(Array(1,2,2)))
  println(minIncrementForUnique(Array(3,2,1,2,1,7)))
}
