package matrix

/*
You are given an m x n binary matrix grid.
An island is a group of 1's (representing land) connected 4-directionally (horizontal or vertical.)
You may assume all four edges of the grid are surrounded by water.
An island is considered to be the same as another if and only if one island can be translated (and not rotated or reflected) to equal the other.
Return the number of distinct islands
 */
object NumberOfDistinctIsland extends  App {


//  def numDistinctIslands(grid: Array[Array[Int]]): Int = {
//    // tells if given point is part of an element or not
//    def isIsland(x: Int, y: Int): Boolean = grid(x)(y) == 1
//    // given a point returns all its valid neighbors
//
//    def getNeighbors(x: Int, y: Int): List[(Int,Int)] = {
//      List((x+1,y), (x-1, y), (x,y+1), (x, y-1), (x+1, y+1), (x-1, y-1), (x+1, y-1), (x-1, y+1)).filter { case (i,j) =>
//        i >= 0 && j >= 0 && i < grid.length && j < grid.head.length && grid(i)(j)== 1
//      }
//    }
//
//    // once a point is identified as part of island,
//    // identifies all other points which are part of same island
//    // put them in list  return it
//    // Also mark them non island in the original matrix
//    def markIsland(x: Int, y: Int): List[(Int,Int)] = {
//      grid(x)(y) = 0
//      var res = Array((x,y))
//      val neighbors = getNeighbors(x,y)
//      neighbors foreach { neighbor =>
//        res :+= neighbor
//        grid(neighbor._1)(neighbor._2) = 0
//
//      }
//
//    }
//
//    // compare two islands and
//    def sameIsland(island1: List[(Int, Int)], island2: List[(Int, Int)]): Boolean = ???
//
//    var islands = Array.empty[List[(Int, Int)]]
//
//    grid.indices foreach { row =>
//      grid(row).indices foreach { col =>
//        if (isIsland(row,col)) {
//          val elementsInIsland = markIsland(row, col)
//          islands.foldLeft(false) { (similarIslandFound, existingIsland) =>
//            if (similarIslandFound) {similarIslandFound} else {
//              if (sameIsland(existingIsland, elementsInIsland)) true
//              else {
//                islands :+= elementsInIsland
//                similarIslandFound
//              }
//            }
//          }
//        }
//      }
//    }
//    islands.length
//
//  }

}
