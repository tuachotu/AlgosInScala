package Searching

object FindFirstAndLastPositionInSortedArray extends App {
  def findPos(numbers: List[Int], target: Int): (Int, Int) = {

    def findPosInternal(left: Int, right: Int, findStart: Boolean): Int = {
      if (left > right) -1
      else {
        val mid = (left + right) / 2
        mid match {
          case _ if numbers(mid) > target => findPosInternal(left, mid -1, findStart)
          case _ if numbers(mid) < target => findPosInternal( mid + 1,right, findStart)
          case _ if findStart == true =>
             if (mid == left || numbers(mid-1) != target)  mid
             else findPosInternal(left, mid-1, findStart)
          case _ => //findStart == false
            if (mid == right || numbers(mid+1) != target) mid
            else findPosInternal( mid + 1, right, findStart)
        }
      }
    }

    val startPos = findPosInternal(0,numbers.length-1, true)

    if (startPos == -1) (-1,-1) else (startPos, findPosInternal(0,numbers.length-1, false))}

  println(findPos(List(1,2,3,4,5,6,6,6,7,8,9,10), 6))
  println(findPos(List(1,2,3,4,5,6,6,6,7,8,9,10), 1))
  println(findPos(List(1,2,3,4,5,6,6,6,7,8,9,10), 16))


}
