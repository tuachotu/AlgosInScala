package sorting

object MergeKSortedLists extends App {
  def mergeKLists(lists: List[List[Int]]): List[Int] ={
    val validLists = lists.filter(_.nonEmpty)
    if (validLists.isEmpty) Nil else {
      val firstElementsSorted = validLists.map(_.head).sorted
      val restOfElement = (firstElementsSorted.tail :: validLists.map(_.tail)).filter(_.nonEmpty)
      val smallestElement = firstElementsSorted.head
      if (restOfElement.isEmpty) List(smallestElement) else smallestElement::mergeKLists(restOfElement)
    }
  }

  println(mergeKLists(List(List(1,4,5),List(1,3,4),List(2,6))).mkString(","))
  println(mergeKLists(List(List(1,4,5),List(1,3,4),Nil)).mkString(","))


}
