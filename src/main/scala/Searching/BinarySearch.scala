package Searching

object BinarySearch extends App {
  //
  def binarySearch(list: List[Int], value: Int): Option[Int] = {
    def binarySearchInternal(start: Int, end: Int): Option[Int] = {
      if (start > end)  None
      (start + end)/2 match {
        case m if value < list(m) => binarySearchInternal(start, m-1)
        case m if value > list(m) => binarySearchInternal(m+1, end)
        case m  => Some(m)
      }
    }
    binarySearchInternal(0, list.length - 1 )
    }
    
    println(binarySearch(List(1,2,3,4,5,6,7,8), 3))
  }


