package array

object RotateArrayByN extends App {
  def rotateByN(l: List[Int], n: Int): List[Int] = {
    val rightSide = l.take(l.length-n)
    val leftSide = l.drop(l.length-n)

    leftSide ++ rightSide
  }

  println(rotateByN(List(1,2,3,4,5,6,7,8,9), 4).mkString(","))
  println(rotateByN(List(1,2,3,4,5,6,7,8,9), 1).mkString(","))

}
