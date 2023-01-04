package Searching

import scala.collection.mutable.PriorityQueue

object KthLargestElement extends  App {
  collection.mutable.PriorityQueue(7, 5, 2, 3, 1)(Ordering[Int].reverse).dequeueAll

  def findKthElementHeap(l: List[Int], k: Int): Option[Int] = {
    if (l.isEmpty) None
    else {
      val myMaxHeap = PriorityQueue[Int]()
      l foreach( myMaxHeap.enqueue(_))
      val result = (0 until k).foldLeft(0) { (result, index) => myMaxHeap.dequeue()}
      Some(result)

    }
  }

  def findKthSmallestElementHeap(l: List[Int], k: Int): Option[Int] = {
    if (l.isEmpty) None
    else {
      val myMaxHeap = PriorityQueue[Int]()(Ordering[Int].reverse)
      l foreach (myMaxHeap.enqueue(_))
      val result = (0 until k).foldLeft(0) { (_, _) => myMaxHeap.dequeue() }
      Some(result)

    }
  }

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

  println(findKthElement(List(9,8,7,6,5,4,3,2,1), 9-3+ 1))
  println(findKthElement(List(8,9,7,5,6,4,2,3,1), 9-3+ 1))
  println(findKthElement(List(9, 8, 7, 6, 5, 4, 3, 2, 1), 3 ))
  println(findKthElement(List(8, 9, 7, 5, 6, 4, 2, 3, 1), 3))
//  println(findKthElement(List(9,8,7,6,5,4,3,2,1), 3))
//  println(findKthElementHeap(List(9,8,7,6,5,4,3,2,1), 3))
//  println(findKthSmallestElementHeap(List(9,8,7,6,5,4,3,2,1), 3))

}
