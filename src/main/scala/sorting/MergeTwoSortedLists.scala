package sorting

object MergeTwoSortedLists extends App {
  def mergeSortedLists(l1: List[Int], l2:List[Int]): List[Int] = {
    (l1,l2) match {
      case (Nil, Nil) => Nil
      case (l1, Nil) => l1
      case (Nil, l2) => l2
      case (xl1::l1Tail, yl2::l2Tail) =>
        if (xl1 < yl2) xl1::mergeSortedLists(l1Tail,l2)
        else yl2::mergeSortedLists(l2Tail, l1)
    }
  }

  println(mergeSortedLists(List(1,3,5,7,9), List(2,4,6,8,10)).mkString(","))

}
