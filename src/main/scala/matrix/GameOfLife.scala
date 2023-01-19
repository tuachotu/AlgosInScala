package matrix

object GameOfLife extends App {
  def gameOfLife(board: Array[Array[Int]]): Unit = {
    val M = board.length
    val N = board.head.length

    def neighbors(x:Int, y:Int):List[(Int,Int)] = {
      List((x+1,y),(x-1,y),(x,y+1),(x,y-1),(x+1,y+1),(x+1,y-1),(x-1,y+1),(x-1,y-1))filter { pos =>
        pos._1 >=0 && pos._1 < M && pos._2 >= 0 && pos._2 < N
      }
    }

    def neighborState(x:Int, y:Int): (Int,Int) = {
      neighbors(x,y).foldLeft((0,0)) { case ((dead, alive), (x,y)) =>
        board(x)(y) match {
          case 0 => (dead+1, alive)
          case _ => (dead, alive+1)
        }
      }
    }

    def newState(x:Int, y:Int):Int = {
      val (deadNeighbor, aliveNeighbor) = neighborState(x,y)
      board(x)(y) match {
        case 0 if aliveNeighbor == 3 => 1
        case 1 if aliveNeighbor > 3 || aliveNeighbor < 2 => 0
        case 1 if aliveNeighbor == 3 || aliveNeighbor == 2 => 1
        case p => p
      }
    }

    val res = board.indices map { row =>
      board(row).indices map { col =>
        newState(row,col)
      }
    }

    res.foreach(row  => println(row.mkString(",")))
  }

  val input1 = Array(Array(0,1,0),Array(0,0,1),Array(1,1,1),Array(0,0,0))

  input1.foreach(row  => println(row.mkString(",")))
  println("========================")
  gameOfLife(input1)

  println("========================")
  val input2 = Array(Array(1, 1), Array(1,0))

  input2.foreach(row => println(row.mkString(",")))
  println("========================")
  gameOfLife(input2)
}
