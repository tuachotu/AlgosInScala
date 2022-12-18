package array


/*
Given an array of integers nums and an integer k, return the total number of subarrays whose sum equals to k.

Input: nums = [1,1,1], k = 2
Output: 2

Input: nums = [1,2,3], k = 3
Output: 2
 */

object SubarraySumEqualsK extends App{
  def findArrayCountBruteForce(nums:List[Int], target: Int): Int = {  // O(n3)
    val sums = for {
      i <- nums.indices
      j <- i until nums.length
    } yield  /*nums.drop(i).take(j-i+1)*/ nums.slice(i,j+1).sum

    sums.count(_ == target)
  }

  // populate an array with sum till  each index
  // if difference of sum at each index is same as target then that index pair is result
  def findArrayCountWithSumWithSpace(nums:List[Int], target: Int): Int = {
    val lookup = nums.foldLeft(List[Int](0)) { (lookupInProgress, number) =>
       lookupInProgress ++ List(lookupInProgress.last + number)
    } // n + 1 size
    val res =  for {
      i <- nums.indices
      j <- i+1 to nums.length // tricky part
    } yield lookup(j) - lookup(i)
    res.count(_ == target)
  }

  def findArrayCountWithSumWithOutSpace(nums:List[Int], target: Int): Int = {
    nums.indices.foldLeft(0/*count*/, 0, Map[Int, Int]((0 -> 1))/*SumCountMap*/) { case ((count, sum, sumCountMap), index) =>
      val newSum = nums(index) + sum
      val newCount =  sumCountMap.getOrElse(newSum - target, 0) + count
      val newSumCountMap = sumCountMap + (newSum -> (sumCountMap.getOrElse(newSum, 0) + 1) )
      (newCount, newSum, newSumCountMap)
    }._1
  }


  println(findArrayCountBruteForce(List(1,1,1), 2))
  println(findArrayCountWithSumWithSpace(List(1,1,1), 2))
  println(findArrayCountWithSumWithOutSpace(List(1,1,1), 2))
  println
  println(findArrayCountBruteForce(List(1,2,3), 3))
  println(findArrayCountWithSumWithSpace(List(1, 2,3), 3))
  println(findArrayCountWithSumWithOutSpace(List(1,2,3), 3))
//  println
//  println(findArrayCountBruteForce(List(1,2,3,15), 15))
//  println(findArrayCountWithSumWithSpace(List(1,2,3,15), 15))
//  println
//  println(findArrayCountBruteForce(List(1,-1, 2,3,15, -12,-3), 0))
//  println(findArrayCountWithSumWithSpace(List(1,-1, 2,3,15, -12,-3), 0))

}
