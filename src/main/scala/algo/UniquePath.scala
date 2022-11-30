package algo

object UniquePath extends App {

  def uniquePath(m: Int, n: Int): Int = {
    def isValidPos(r: Int, c: Int): Boolean = r >= 0 && r < m && c >= 0 && c < n
    def getMoves(r: Int, c: Int): List[(Int, Int)] ={
        List((r+1, c), (r,c+1)).filter(p=> isValidPos(p._1, p._2))
    }
    def isTarget(r:Int, c:Int):Boolean = r == m-1 && c == n-1
    def uniquePathInternal(x: Int, y:Int): Int = {
      if (isValidPos(x,y)) {
        val possibleMoves = getMoves(x,y)
        val pathFoundCount =  possibleMoves.count(pos => isTarget(pos._1, pos._2))
        val yetToSearchPath = possibleMoves.filter(pos => !isTarget(pos._1, pos._2))

        pathFoundCount + yetToSearchPath.foldLeft(0) { (pathCout, pos) => pathCout + uniquePathInternal(pos._1, pos._2)}
      } else 0
    }
    if (m == 1 && n ==1) 1 else uniquePathInternal(0,0)
  }

  println(uniquePath(3,7))
  println(uniquePath(23,12))

}
