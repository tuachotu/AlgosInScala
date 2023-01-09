package algo

object MinimumLimitOfBallsInABag extends App{

  def minimumSize(nums: List[Int], maxOperations: Int): Int = {
    def maxSizeOpCountValid(maxSize: Int): Boolean = {
      val nnededOps = nums.foldLeft(0) { (opCount, num) =>
        val ops = num/maxSize
        if (num%maxSize == 0) opCount + ops- 1
        else opCount + ops
      }
      nnededOps <= maxOperations
    }

    def minimumSizeInternal(start: Int,  end: Int ): Int = {
      if (start > end) 0
      else {
          val mid = start + (end - start) /2
           if (maxSizeOpCountValid(mid)) {
             val nextVal = minimumSizeInternal(start, mid -1)
             if (nextVal == 0) mid else nextVal
           } else {
             minimumSizeInternal(mid + 1, end)
           }
      }
    }

    minimumSizeInternal(0, Int.MaxValue)
  }

  println(minimumSize( List(9), 2))
  println(minimumSize( List(2,4,8,2), 4))

}
