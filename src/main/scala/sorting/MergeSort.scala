package sorting

object MergeSort extends App {
  def merge(left: List[Int], right: List[Int]): List[Int] = {
    (left, right) match {
      case (Nil, Nil) => Nil
      case (Nil, l2) => l2
      case (l1, Nil) => l1
      case (firstItemInL1::restOfL1, firstItemInL2::restOfL2) =>
        if (firstItemInL2 > firstItemInL1) firstItemInL1::merge(restOfL1, right)
        else firstItemInL2::merge(restOfL2, left)
    }
  }

  def mergeSort(l:List[Int]): List[Int] = {
    val mid = l.length / 2
    if (mid == 0) l else merge(mergeSort(l.take(mid)), mergeSort(l.drop(mid)))
  }

  println(mergeSort(List(9,8,7,6,5,4,3,2,1)).mkString(","))


}
