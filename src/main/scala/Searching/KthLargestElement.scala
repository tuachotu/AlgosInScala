package Searching

object KthLargestElement extends  App {

  def findKthElement(l: List[Int], k: Int): Option[Int] = {
    if (l.isEmpty ) None //TODO: Check for boundary
    else {
      val pivot = l.last

      val newList = l.filter(_ < pivot) ++ l.filter(_ == pivot) ++ l.filter(_ > pivot)
      val pivotPos = newList.find(_ == pivot).get
      if (pivotPos == k) Some(pivot)
      else if (pivotPos > k) findKthElement(newList.filter(_ < pivot), k)
      else findKthElement(newList.filter(_ > pivot), k)
    }
  }

  println(findKthElement(List(9,8,7,6,5,4,3,2,1), 9-6))

}
