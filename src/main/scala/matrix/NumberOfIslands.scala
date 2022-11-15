package matrix

import scala.collection.mutable.ArrayBuffer

object NumberOfIslands extends App {
  val grid = ArrayBuffer(ArrayBuffer(1, 1, 0, 0, 0), ArrayBuffer(1, 1, 0, 0, 0), ArrayBuffer(0, 0, 0, 1, 1), ArrayBuffer(0, 0, 1, 0, 0), ArrayBuffer(1, 1, 0, 0, 0))

  for (i <- 0 until  5) {
    for (j <- 0 until 5) {
      print(grid(i)(j));
      print(" ")
    }
    println
  }


  println
  println
  println

  var  islandCount: Int = 0
  for (i <- 0 until  5) {
    for (j <- 0 until 5) {
      if (grid(i)(j) == 1) {
        islandCount = islandCount + markIsland(i, j)
      }
    }
  }

  def markIsland(i: Int, j: Int): Int = {
    if (i < 0 ||  j < 0 || i > 4 || j > 4 || grid(i)(j) == 0 )  {
      0
    } else {
      grid(i)(j) = 0
      markIsland(i+1, j)
      markIsland(i, j+1)
      markIsland(i-1,j)
      markIsland(i,j-1)
      1
    }
  }

  println(islandCount)
}
