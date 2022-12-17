package algo
// Boyer–Moore majority vote algorithm
object MajorityElelment extends App {
  def findMaj(l:List[Int]): Int = {
    l.indices.foldLeft((0,-1)) { case((eCount, majElement), index) =>
      if (eCount == 0) {
        (1, l(index))
      } else if (majElement == l(index)) {
        (eCount + 1, majElement)
      } else {
        (eCount - 1, majElement)
      }
    }._2
  }

  println(findMaj(List(1, 8,8, 7, 4, 1, 2, 2,8,8,8, 2, 2, 2, 2)))

}
