package algo

//You are given an integer array height of length n. There are n vertical lines drawn such that the two endpoints of the ith line are (i, 0) and (i, height[i]).
//Find two lines that together with the x-axis form a container, such that the container contains the most water.
//Return the maximum amount of water a container can store.
object ContainerWithMostWater extends App {

  def calculateMaxArea(l:List[Int]): Int = {
    val indexs = for {
      i <- (0 until l.length)
      j <- i until l.length
    } yield ((j-i) * Math.min( l(i),l(j)))
    indexs.max
  }

  def CalculateMaxAreaFaster(l:List[Int]): Int = {
    var left = 0
    var right = l.length -1
    var maxArea = 0

    while (left < right) {
      val area = (right - left) * Math.min(l(left), l(right))
      if (area > maxArea) maxArea = area
      if (l(left) > l(right)) right = right -1 else left = left + 1
    }

    maxArea
  }

  println(calculateMaxArea(List(1,8,6,2,5,4,8,3,7))) // Should be 49
  println(CalculateMaxAreaFaster(List(1,8,6,2,5,4,8,3,7))) // Should be 49
  println(calculateMaxArea(List(1,1))) // Should be 1
  println(CalculateMaxAreaFaster(List(1,1))) // Should be 1

}
