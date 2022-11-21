package matrix

object QueenAttack extends App {
  def markQueenAttack(n: Int, m: Int, rQ: Int, cQ: Int) : Unit = {
    def isQueenPos(r:Int, c:Int): Boolean = {
      r == rQ || c == cQ || (rQ + cQ == r + c) || (rQ - cQ == r - c)
    }
    0 until n foreach { i =>
      0 until m foreach { j =>
        (i,j) match {
          case _ if i == rQ && j == cQ => print("Q ")
          case _ if isQueenPos(i, j) => print("X ")
          case _ => print("_ ")
        }
      }
      println
    }
  }

  markQueenAttack(5,5,1,4)

}
