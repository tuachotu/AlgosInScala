package array

/*
Given an array of integers, and a number ‘sum’, print all pairs in the array whose sum is equal to ‘sum’.
Input  :  arr[] = {1, 5, 7, -1, 5},
          sum = 6
Output : (1, 5) (7, -1) (1, 5)

Input  :  arr[] = {2, 5, 17, -1},
          sum = 7
Output :  (2, 5)
 */


/*
=== rough
Map targetSum -> Index
target = 6
index number lookupNumber lookupMap res
index  = 0 number = 1  storeNumber = 5,  lookupMap = [5 -> List(0)]   res = ()
index  = 1 number = 5  storeNumber = 1,  lookupMap = [5 -> List(0), 1-> List(1)]   res =List((0,1))
index = 2  number = 7  storeNuber = -1  lookupMap = [5 -> List(0), 1-> List(1), -1 -> List(2)]  res = List((0,1))

 */
object AllPAirGivenSum extends App {
  def findPairs(numbers: List[Int], target: Int): List[(Int, Int)] = {
    numbers.indices.foldLeft(Map[Int, List[Int]](), List.empty[(Int,Int)]) { case((lookupMap, res), index) =>
      val newRes = if (lookupMap.contains(numbers(index))) {res ++ lookupMap(numbers(index)).map( x => (x, index)) } else res
      val newMap =  lookupMap + ((target - numbers(index)) -> (index::lookupMap.getOrElse(target - numbers(index), List.empty[Int])))
      (newMap,newRes)
    }._2
  }
  println(findPairs(List(1, 5, 7, -1, 5), 6))
}
