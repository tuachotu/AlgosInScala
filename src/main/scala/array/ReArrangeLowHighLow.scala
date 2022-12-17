package array

/*
Given an integer array, rearrange it such that every second element becomes greater than its left and right elements.
Assume no duplicate elements are present in the array.
Input:  {1, 2, 3, 4, 5, 6, 7}
Input:  {9, 6, 8, 3, 7}
Input:  {6, 9, 2, 5, 1, 4}

 */
object ReArrangeLowHighLow extends App{
  def reArrange(nums : List[Int]): List[Int] = {
    def makeArray(l: List[Int]): List[Int] = {
      if (l.isEmpty || l.length == 1) l else l.head::l.last::makeArray(l.tail.init)
    }
    makeArray(nums.sorted) // nlogn
  }

  def reArrangeFast(nums : List[Int]): List[Int] = {
    (1 until nums.length by 2).toList.foldLeft(nums) { case (result, index) =>
      val temp = if (result(index -1) > result(index)) {
        result.take(index-1) ++ (result(index)::result(index-1)::result(index+1)::result.drop(index+2))
      } else result
      if ( (index < nums.length-1) && temp(index + 1) > temp(index)) {
        temp.take(index-1) ++ (temp(index-1)::temp(index+1)::temp(index)::temp.drop(index+2))
      } else temp
    }
  }

  println(reArrangeFast(List(1, 2, 3, 4, 5, 6, 7)))
  println(reArrangeFast(List(9, 6, 8, 3, 7)))
  println(reArrangeFast(List(6, 9, 2, 5, 1, 4)))
//  println(reArrange(List(1, 2, 3, 4, 5, 6, 7)))
//  println(reArrange(List(9, 6, 8, 3, 7)))
//  println(reArrange(List(6, 9, 2, 5, 1, 4)))

}
