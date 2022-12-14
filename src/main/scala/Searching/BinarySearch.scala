package Searching

import scala.annotation.tailrec

object BinarySearch extends App {
  //
  def binarySearch(list: List[Int], value: Int): Option[Int] = {
    @tailrec
    def binarySearchInternal(start: Int, end: Int): Option[Int] = {
      if (start >= end) {  None } else {
        start + ((end -start)/2) match {
        //(start + end) / 2 match {
          case m if value < list(m) => binarySearchInternal(start, m - 1)
          case m if value > list(m) => binarySearchInternal(m + 1, end)
          case m => Some(m)
        }
      }
    }
    binarySearchInternal(0, list.length - 1 )
    }
    
    //println(binarySearch(List(1,2,3,4,5,6,7,8), 3))
//    println(binarySearch(List(1,2,3,4,5,6,7,8), 12))
//    println(binarySearch(List(1,2,3,4,5,6,7,8), 0))
//    println(binarySearch(List(1,2,3,4,5,6,7,8), 7))
  println(binarySearch(List(1,2,3,4,5,6,7,8,9), 7))
  }


