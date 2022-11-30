package matrix


// rotatte by 180 means
// - reverse the rows (i.e. 0 - n ---> n - 0
// - and then reverse each row
object RotoateBy180 extends App {
  def rotate(mat:List[List[Int]]): List[List[Int]] = {
    (mat.reverse).map(_.reverse)
  }

  val input = List(List(1, 2, 3, 4, 5), List(6, 7, 8, 9, 10), List(11, 12, 13, 14, 15), List(16, 17, 18, 19, 20), List(21, 22, 23, 24, 25))
  input.foreach(row => println(row.mkString("  ")))
  println()
  rotate(input).foreach(row => println(row.mkString("  ")))
}
