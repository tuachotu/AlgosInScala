package algo

object SlidingPuzzle extends  App {
  val alreadyChecked = scala.collection.mutable.ListBuffer.empty[String]
  def slidingPuzzle(board: List[List[Int]]): Int = {
    def swapPosition(x: Int, y: Int): List[(Int, Int)] = {
      List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)).filter(p =>
        p._1 >= 0 && p._1 < 2 && p._2 >= 0 && p._2 < 3
      )
    }

    def findEmpty(board: List[List[Int]]): (Int, Int) = {
      val res = board.foldLeft(0, 0, false) { case ((pos_x, pos_y, found), elems) =>
        if (found) (pos_x, pos_y, found)
        else {
          elems.indices.find(elems(_) == 0) match {
            case Some(index) => (pos_x, index, true)
            case None => (pos_x + 1, 0, false)
          }
        }
      }
      (res._1, res._2)
    }

    def boardSolved(state: List[List[Int]]): Boolean = {
      state == List(List(1, 2, 3), List(4, 5, 0))
    }

    def nextState(nonEmptyCell: (Int, Int), emptyCell: (Int, Int)): List[List[Int]] = {
      val nonEmptyValue = board(nonEmptyCell._1)(nonEmptyCell._2)
      board.indices.toList map { x =>
        board(x).indices.toList map { y =>
          (x, y) match {
            case (nonEmptyCell._1, nonEmptyCell._2) => 0
            case (emptyCell._1, emptyCell._2) => nonEmptyValue
            case _ => board(x)(y)
          }
        }
      }
    }

    def boardToString(state: List[List[Int]]): String = state.map(_.mkString(",")).mkString(",")

    if (!alreadyChecked.contains(boardToString(board))) {
      alreadyChecked += boardToString(board)
      if (boardSolved(board)) 1 else {
        val emptyCell = findEmpty(board)
        val nextPositions = swapPosition(emptyCell._1, emptyCell._2)
        val nextPositionsSwapped = nextPositions.map { pos => nextState(pos, emptyCell) }.filter(p => !alreadyChecked.contains(boardToString(p)))
        if (nextPositionsSwapped.isEmpty)-1
        else if (nextPositionsSwapped.exists(boardSolved)) {
          println("found")
          1
        }
        else  {
          1 + nextPositionsSwapped.map(slidingPuzzle).min

        }
      }
    } else 0
    }


  //println(slidingPuzzle(List(List(1,2,3), List(4,0,5))))
  println(slidingPuzzle(List(List(4,1,2), List(5,0,3))))
}