package matrix

object MatrixSameShape extends App {




  def sameShape(shape1: Array[(Int,Int)], shape2: Array[(Int,Int)]): Boolean = {
    def moveShapeLeftTop(shape: List[(Int,Int)]): List[(Int, Int)] = {
      val leftMostPoint = shape.tail.foldLeft(shape.head._1) { case(knownLeftMostPoint, point) => Math.min(knownLeftMostPoint,point._1)}
      val topMostPoint = shape.tail.foldLeft(shape.head._2) { case(knownTopMostPoint, point) => Math.min(knownTopMostPoint,point._2)}
      shape map { point => (point._1 - leftMostPoint, point._2 - topMostPoint) }
    }
    moveShapeLeftTop(shape1.toList).sortBy(_._1).sortBy(_._2) == moveShapeLeftTop(shape2.toList).sortBy(_._1).sortBy(_._2)

  }

  val s1 = Array((1,0),(1,1),(2,1),(2,2))
  val s2 = Array((4,2),(4,3),(5,4),(5,3))

  val s3 = Array((4,2),(4,3),(5,3),(3,3))
  val s4 = Array((1,0),(1,1),(2,1),(0,1))
  val grid = Array(Array(0, 1, 0, 0, 0), Array(1, 1, 0, 0, 0), Array(0, 1, 0, 0, 0), Array(0, 0, 0, 1, 0), Array(0, 0, 1, 1, 0), Array(0, 0, 0, 1, 0))
  grid foreach { row => println(row.mkString(",")) }
  println()
  println(sameShape(s1,s2))
  println(sameShape(s1,s3))
  println(sameShape(s4,s3))

}
