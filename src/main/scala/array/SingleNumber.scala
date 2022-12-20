package array

/*
Given a non-empty array of integers nums, every element appears twice except for one.
Find that single one.
Example 1:
Input: nums = [2,2,1]
Output: 1

Example 2:
Input: nums = [4,1,2,1,2]
Output: 4

Example 3:
Input: nums = [1]
Output: 1
 */

object SingleNumber extends App {
  // search each element in the list, and return on first unique
  def findSingleNumLong(nums: List[Int]): Int ={
    var i = 0
    var result: Option[Int] = None
      nums.indices.takeWhile { index =>
        if ( !(nums.drop(index+1) ++ nums.take(index)).contains(nums(index))) {
          result = Some(nums(index))
          false
        } else {
          true
        }
      }

    result.get
  }

  def findSingleMath(nums: List[Int]): Int =
    (nums.foldLeft(Set.empty[Int]) { _ + _ }.sum *2) - nums.sum


  def findSingleConstantSpace(nums: List[Int]): Int = nums.tail.foldLeft(nums.head) {_ ^ _}


  println(findSingleNumLong(List(4,1,2,1,2)))
  println(findSingleMath(List(4,1,2,1,2)))
  println(findSingleConstantSpace(List(4,1,2,1,2)))
  println
  println(findSingleNumLong(List(2)))
  println(findSingleMath(List(2)))
  println(findSingleConstantSpace(List(2)))
  println
  println(findSingleNumLong(List(2,2,1)))
  println(findSingleMath(List(2,2,1)))
  println(findSingleConstantSpace(List(2,1,2)))
}
