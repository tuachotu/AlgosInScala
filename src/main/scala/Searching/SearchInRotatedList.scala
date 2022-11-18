package Searching

object SearchInRotatedList extends App {


  def findInRotated(list: List[Int], value: Int): Option[Int] = {

    def rotationPoint(index:Int): Boolean =
      index > 0 && index < list.length && list(index) < list(index-1)


    def findInRotatedInternal(start: Int, end: Int): Option[Int] = {
      if (start > end) {
        None
      } else {
        (start + end)/2 match {
          case mid if list(mid) > value => findInRotatedInternal(start, mid-1)
          case mid if list(mid) < value => findInRotatedInternal(mid + 1, end)
          case mid if rotationPoint(mid) && value > list(mid) => findInRotatedInternal(0, mid-1)
          case mid if rotationPoint(mid) && value < list(mid) => findInRotatedInternal(mid + 1, list.length-1)
          case mid => Some(mid)
        }
      }
    }
    findInRotatedInternal(0, list.length-1)
  }

  println(findInRotated(List(6,7,8,9,1,2,3,4,5),1))
  println(findInRotated(List(6,7,8,9,1,2,3,4,5),5))
  println(findInRotated(List(1,2,3,4,5,6,7,8,9),3))
  println(findInRotated(List(1,2,3,4,5,6,7,8,9),12))
  println(findInRotated(List(1,2,3,4,5,6,7,8,9),0))
}
