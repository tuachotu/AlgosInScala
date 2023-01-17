package matrix

object NumberOfIslands extends App {
val grid = Array(Array(1, 1, 0, 0, 0), Array(1, 1, 0, 0, 0), Array(0, 0, 0, 1, 1), Array(0, 0, 1, 0, 0), Array(1, 1, 0, 0, 0))

  for (i <- 0 until 5) {
    for (j <- 0 until 5) {
      print(grid(i)(j));
      print(" ")
    }
    println
  }


  def countIsland(grid: Array[Array[Int]]): Int = {
    def neighbors(x: Int, y: Int): List[(Int,Int)] = {
      List((x+1,y),(x-1,y),(x,y+1),(x,y-1)).filter { case (i,j) => i < grid.length && i >= 0 && j >=0 && j < grid.head.length && grid(i)(j) == 1}
    }
    def exploreIsland(x: Int, y: Int):List[(Int,Int)] = {
      val yetToVisit = scala.collection.mutable.Stack.empty[(Int,Int)]
      val visited =  scala.collection.mutable.HashSet.empty[(Int,Int)]

      yetToVisit.push((x,y))
      while(yetToVisit.nonEmpty) {
        val node = yetToVisit.pop()
        grid(node._1)(node._2) = 0
        if (!visited.contains(node)) {
          visited += node
          neighbors(node._1, node._2).foreach(yetToVisit.push)
        }
      }
      visited.toList
    }
    var islandCount = 0
    for {
      row<- 0 until grid.length
      col <- 0 until grid.head.length
    } {
      if (grid(row)(col) == 1) {
        //println(exploreIsland(row,col).mkString(","))
        exploreIsland(row,col)
        islandCount = islandCount + 1
      }
    }
    islandCount
  }

  def countIslandV2(grid: Array[Array[Int]]): Int = {
    def neighborsV2(x: Int, y: Int): List[(Int, Int)] = {
      List((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)).filter { case (i, j) => i < grid.length && i >= 0 && j >= 0 && j < grid.head.length && grid(i)(j) == 1 }
    }

    def markIsland(x:Int, y:Int): Unit = {
      grid(x)(y) = 0
      neighborsV2(x,y) foreach { case(i,j) =>  markIsland(i,j)}
    }

    var count = 0
    for {
      row <- 0 until grid.length
      col <- 0 until grid.head.length
    } {
      if (grid(row)(col) == 1) {
        markIsland(row,col)
        count = count +1
      }
    }
    count
  }

  //println(countIsland(grid))
  println(countIslandV2(grid))
}
