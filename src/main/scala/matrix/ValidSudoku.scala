package matrix

object ValidSudoku extends App {
  val AllowedChars = List("1","2","3","4","5","6","7","8","9")
  def validSudokuGrid(matrix: List[List[String]]) : Boolean = {
    def getCol(col: Int): List[String] = matrix.map(_ (col))
    def getRow(row: Int): List[String] = matrix(row)
    def getSubBoxStart(row: Int, col: Int): (Int, Int) = (3 * (row / 3), 3 * (col / 3))
    def getSubBox(row:Int, col:Int): List[List[String]]= {
      (0 until 3).toList map { r =>
        (0 until 3).toList map { c =>
          matrix(row + r)(col + c)
        }
      }
    }

    def validatePoint(r:Int, c: Int): Boolean = {
      val numbersInCol = getCol(c).filter(c => AllowedChars.contains(c))
      val numbersInRow = getRow(r).filter(c => AllowedChars.contains(c))
      val subBoxStart = getSubBoxStart(r,c)
      val numbersInSubBox = getSubBox(subBoxStart._1, subBoxStart._2).flatten.toList.filter(c => AllowedChars.contains(c))

      numbersInSubBox.distinct.length == numbersInSubBox.length && numbersInRow.distinct.length == numbersInRow.length && numbersInCol.distinct.length == numbersInCol.length
    }

    validatePoint(8,0)

  }

  val input = List(List("5", "3", ".", ".", "7", ".", ".", ".", ".")
    , List("6", ".", ".", "1", "9", "5", ".", ".", ".")
    , List(".", "9", "8", ".", ".", ".", ".", "6", ".")
    , List("8", ".", ".", ".", "6", ".", ".", ".", "3")
    , List("4", ".", ".", "8", ".", "3", ".", ".", "1")
    , List("7", ".", ".", ".", "2", ".", ".", ".", "6")
    , List(".", "6", ".", ".", ".", ".", "2", "8", ".")
    , List(".", ".", ".", "4", "1", "9", ".", ".", "5")
    , List(".", ".", ".", ".", "8", ".", ".", "7", "9"))


  val inputBad = List(List("5", "3", ".", ".", "7", ".", ".", ".", ".")
    , List("6", ".", ".", "1", "9", "5", ".", ".", ".")
    , List(".", "9", "8", ".", ".", ".", ".", "6", ".")
    , List("8", ".", ".", ".", "6", ".", ".", ".", "3")
    , List("4", ".", ".", "8", ".", "3", ".", ".", "1")
    , List("7", ".", ".", ".", "2", ".", ".", ".", "6")
    , List(".", "6", ".", ".", ".", ".", "2", "8", ".")
    , List(".", ".", ".", "4", "1", "9", ".", ".", "5")
    , List("6", ".", ".", ".", "8", ".", ".", "7", "9"))


  //println(validSudokuGrid(input))
  println(validSudokuGrid(inputBad))

}
