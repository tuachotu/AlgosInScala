package matrix

// given a matrix find steps from source to Destination
import  scala.collection.mutable.HashSet
object FindAllPaths extends App {
  def findAllPaths(grid: List[List[Int]]): List[String] = {
    def validPos(x:Int, y:Int): Boolean = x < grid.length && x >= 0 && y < grid.length && y >= 0
    def isBlocker(x:Int, y:Int): Boolean = grid(x)(y) == 0
    def getValidMoves(x:Int, y:Int): List[(Int, Int)] = {
      List((x+1,y),(x-1,y),(x,y+1),(x,y-1)).filter (move => validPos(move._1, move._2) && !isBlocker(move._1, move._2))
    }
    def isTarget(x: Int, y: Int): Boolean = x == grid.length-1 && y == grid.length-1
    def posToString(x: Int, y: Int): String = s"-> ($x,$y) ->"

    val lookup = HashSet[String]()

    def findAllPathsInternal(x: Int, y: Int): List[String] = {
      if(!validPos(x,y)) List.empty[String]
      else {
        if (lookup.contains(posToString(x,y))) List.empty[String]
        else {
          lookup += posToString(x,y)
          val validMoves = getValidMoves(x, y)
          val targetFound = validMoves.filter(move => isTarget(move._1, move._2))
          val pathToExplore = validMoves.filter(move => !isTarget(move._1, move._2))
          val rr = (targetFound, pathToExplore) match {
            case (Nil, Nil) => List[String]("")
            case (_, Nil) => List( posToString(grid.length - 1, grid.length - 1))
            case (Nil, _) => pathToExplore.flatMap {pos =>
              posToString(x, y) :: findAllPathsInternal(pos._1, pos._2) }
            case (_, _) => List(posToString(grid.length - 1, grid.length - 1)) ++ pathToExplore.flatMap(pos => posToString(x, y) :: findAllPathsInternal(pos._1, pos._2))
          }
          rr.filter(_.nonEmpty)
        }
      }
    }

    findAllPathsInternal(0,0)
  }


  val input =   List(
    List(1,0,0,0,0,0),
    List(1,0,0,0,0,0),
    List(1,1,1,0,0,0),
    List(1,0,1,0,0,0),
    List(1,0,1,1,1,0),
    List(1,0,0,0,1,0),
  )

    findAllPaths(input) foreach println



}
