package matrix

import matrix.ReverseMatrixBy90.printMatrix


// first calculate transpose , i.e. Aij = Aji
// then reverse the rows
object ReverseMatrixBy90 extends App {
  def printMatrix(m:List[List[Int]]): Unit = {
    m foreach { row =>
      print(s"${row.mkString(" ") }")
      println
    }


  }

  def rotateBy90(matrix: List[List[Int]], m: Int, n: Int): List[List[Int]] = {
    // first calculate transponse
    val transpose = (0 until n).toList map { col =>
      (0 until m).toList map { row =>
        matrix(row)(col)
      }
    }

    transpose.map(_.reverse)
  }

//  val input = List(List(1,2,3,4,5), List(6,7,8,9,10), List(11,12,13,14,15), List(16,17,18,19,20), List(21,22,23,24,25))
//
//  printMatrix(input)
//  println()
//  println()
//  printMatrix(rotateBy90(input,5,5))
//  println()
//  println()
//
//
//  val input1 = List(List(1,2,3,4,5), List(6,7,8,9,10), List(11,12,13,14,15), List(16,17,18,19,20))
//
//  printMatrix(input1)
//  println()
//  println()
//  printMatrix (rotateBy90(input1, 4, 5))
//  println()
//  println()
//  val input2 = List(List( 2, 3, 4, 5), List( 7, 8, 9, 10), List( 12, 13, 14, 15), List( 17, 18, 19, 20), List( 22, 23, 24, 25))
//
//  printMatrix(input2)
//  println()
//  println()
//  printMatrix(rotateBy90(input2, 5, 4))


  val input21 = List(List(2, 3, 4, 5), List(7, 8, 9, 10))

  printMatrix(input21)
  println()
  println()
  printMatrix(rotateBy90(input21, 2, 4))


}
