package algo

//You are given an integer array height of length n. There are n vertical lines drawn such that the two endpoints of the ith line are (i, 0) and (i, height[i]).
//Find two lines that together with the x-axis form a container, such that the container contains the most water.
//Return the maximum amount of water a container can store.


// Solution
// Area formula
// length = difference between indexes (j -i)
//  width - min (j,i)

// Brute force - calculate area for all pairs and then choose maximum

// Efficient -
// - start accessing array from both ends
// - calculate area at left * right , keep track of max
// - between left rihgt, decrement/increment the smaller



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


  def calculateMaxAreaFunctional(l: List[Int]): Int = {
    def calculateArea(i: Int, j: Int): Int = (j-i)* Math.min(l(i),l(j))

    def  calculateMaxAreaFunctionalInternal(left: Int, right: Int): Int = {
      if (left > right) 0
      else {
        val areaAtIndex = calculateArea(left, right)
        val (newLeft, newRight) = if (l(left) > l(right)) (left, right-1) else (left +1 , right)
        Math.max(areaAtIndex, calculateMaxAreaFunctionalInternal(newLeft, newRight))
      }
    }
    calculateMaxAreaFunctionalInternal(0, l.length -1)
  }

  println(calculateMaxArea(List(1,8,6,2,5,4,8,3,7))) // Should be 49
  println(calculateMaxAreaFunctional(List(1,8,6,2,5,4,8,3,7))) // Should be 49
  println(CalculateMaxAreaFaster(List(1,8,6,2,5,4,8,3,7))) // Should be 49
  println(calculateMaxAreaFunctional(List(1,8,6,2,5,4,8,3,7))) // Should be 49
  println(calculateMaxArea(List(1,1))) // Should be 1
  println(calculateMaxAreaFunctional(List(1,1))) // Should be 1
  println(CalculateMaxAreaFaster(List(1,1))) // Should be 1
  println(calculateMaxAreaFunctional(List(1,1))) // Should be 1

}
