package matrix

object MatrixTranspose extends App {

  //solution - transpose E[row[][col] is swapped with E[col][row]
  def transpose(matrix: List[List[Int]]): List[List[Int]] ={
    val rows = matrix.length
    val cols = matrix.head.length

    (0 until cols).toList map { col =>
      (0 until rows).toList map { row =>
        matrix(row)(col)
      }
    }

  }

  val input = List(List(1,2,3,4,5), List(6,7,8,9,10), List(11,12,13,14,15), List(16,17,18,19,20), List(21,22,23,24,25))
  input.foreach( row => println(row.mkString("  ")))

  println()
  transpose(input).foreach( row => println(row.mkString("  ")))
}
