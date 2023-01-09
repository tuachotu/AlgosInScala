package algo

object FindDuplicateNumber extends App {
  def findDuplicate(nums: List[Int]): Int = {
    nums.sum -(0 until nums.length).sum
  }

  println(findDuplicate(List(1,3,4,2,2)))
  println(findDuplicate(List(3,1,3,4,2)))

}
