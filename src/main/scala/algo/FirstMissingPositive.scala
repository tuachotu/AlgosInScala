package algo

object FirstMissingPositive extends App {

  // - filter out all -ve
  // - 0 to size of remaining array is what will have first missing number
  def firstMissingPositive(nums: Array[Int]): Int = {
    val numsToWorkOn = nums.filter(_>= 0).toList
    val size = numsToWorkOn.length
    var found: Boolean = false
    var i:Int = 0
    while(!found && (i <= size) ) {
      if (numsToWorkOn.contains(i+1)) {
        i = i + 1
      } else {
        found = true
      }
    }

    if (found) i + 1 else -1
  }


  def firstMissingPositiveLinear(nums: Array[Int]): Int = {
    val posNumLookup = nums.filter(_>0).foldLeft(Map.empty[Int,Int]) { case (lookup, num) =>
      lookup + (num -> (lookup.getOrElse(num,0) + 1))
    }
    val posNumCount = nums.count(_>=0)

    (for {
      i <- 1 to posNumCount
      if !posNumLookup.contains(i)
    } yield i).toList match {
      case Nil => 1
      case ll => ll.head
    }
  }


  println(firstMissingPositiveLinear(Array(1, 2, 0)))
  println(firstMissingPositiveLinear(Array(3, 4, -1, 1)))
  println(firstMissingPositiveLinear(Array(7, 8, 9, 11, 12)))
  println
  println
  println
  println(firstMissingPositive(Array(1,2,0)))
  println(firstMissingPositive(Array(3,4,-1,1)))
  println(firstMissingPositive(Array(7,8,9,11,12)))
}
