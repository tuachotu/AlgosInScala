package matrix

object PathWithMinimumEffort extends App{

/*  You are a hiker preparing for an upcoming hike.
    You are given heights, a 2D array of size rows x columns
  , where heights[row][col] represents the height of cell(row, col).
    You are situated in the top -left cell, (0, 0), and you hope to travel to the bottom -right cell
  , (rows - 1, columns - 1) (i.e., 0 - indexed).
    You can move up, down, left, or right
  , and you wish to find a route that requires the minimum effort
  .

  A route 's effort is the maximum absolute difference in heights between two consecutive cells of the route
  .

  Return the minimum effort required to travel from the top -left cell to the bottom - right cell
*/


  //solution1
  // Step 1 - find all paths from top to bottom
  // Step 2 - Choose one with least effort


  def minimumEffortPath(heights: List[List[Int]]): Int = {
    val M = heights.length
    val N = heights.head.length
    val pointAlteradyVisited = scala.collection.mutable.HashSet.empty[(Int,Int)]
    def effortForPath(path: List[(Int,Int)]) : Int = {
       path.tail.foldLeft((0, path.head)) { case ((maxEffort, lastpos), pos) =>
        (Math.max(maxEffort, Math.abs(heights(pos._1)(pos._2) - heights(lastpos._1)(lastpos._2))), pos)
      }._1
    }

    def minEffortsInPaths(paths: List[List[(Int,Int)]]): Int = {
      paths match {
        case head::Nil => effortForPath(head)
        case head::last::Nil => Math.min(effortForPath(head), effortForPath(last))
        case _ =>
          paths.tail.foldLeft(effortForPath(paths.head)) { (lastEffort, currentPath) => Math.min(lastEffort, effortForPath(currentPath))}

    }
    }

    def allPathsFromTopToBootom(): List[List[(Int,Int)]] = {

      def possibleMoves(x:Int, y:Int): List[(Int,Int)] = List((x,y-1),(x,y+1),(x+1,y),(x-1,y)).filter { pos =>
        pos._1 >=0 && pos._1< M && pos._2 >= 0 && pos._2 < N && !pointAlteradyVisited.contains((pos._1, pos._2))
      }

      def isTarget(x:Int, y:Int) = x == M-1 && y == N-1

      def allPathsFromTopToBottomInternal(x:Int, y:Int): List[List[(Int,Int)]] = {
        if (pointAlteradyVisited.contains(x, y)) Nil else {
          pointAlteradyVisited.add((x,y))
          if (isTarget(x, y)) Nil
          else {
            val allMoves = possibleMoves(x, y)
            val targetFound = allMoves.count(p => isTarget(p._1, p._2)) >= 1
            val yetToExplorePaths = allMoves.filter(p => !isTarget(p._1, p._2))
            val completedPath = if (targetFound) List((x, y), (M - 1, N - 1)) else List.empty[(Int, Int)]
            println(x,y,"Calling for  --> ", yetToExplorePaths)
            val otherPaths = yetToExplorePaths.flatMap { p => allPathsFromTopToBottomInternal(p._1, p._2) }.filter(path => path.nonEmpty && isTarget(path.last._1, path.last._2)).map { path => (x, y)::path }
            println(x,y,allMoves, "Completed For --> ", completedPath::otherPaths)
            completedPath::otherPaths
          }
        }
      }
      allPathsFromTopToBottomInternal(0,0)
    }

    minEffortsInPaths(allPathsFromTopToBootom().filter(_.nonEmpty))
  }
  val input1= List (List(1,2,2),List(3,8,2),List(5,3,5))
  println(minimumEffortPath(input1))
}
