package array

object MaximumProductSubArray extends App {
  def findSubArrayBruteForce(numbers: List[Int]): Int = {
    (for {
      i <- numbers.indices
      j <- i until numbers.length
      subarr = numbers.drop(i).take(j-i+1)
    } yield subarr.product).max
  }

  def maxOfThree(a: Int, b:Int, c:Int): Int = Math.max(a, Math.max(b,c))
  def minOfThree(a: Int, b:Int, c:Int): Int = Math.min(a, Math.min(b,c))

  def findSubArrayWithSpace(numbers: List[Int]): Int = {
    if (numbers.isEmpty) 0 else {
      numbers.tail.foldLeft((numbers.head, numbers.head, numbers.head)) { case ((currentMax, currentMin, res), number) =>

        val min = minOfThree(currentMin * number, number, number * currentMax)
        val max = maxOfThree(currentMax * number, number, number * currentMin)
        (max, min, Math.max(res, currentMax))

      }._3
    }


  }


  println(findSubArrayBruteForce(List(-6, 4, -5, 8, -10, 0, 8))) // 1600
  println(findSubArrayWithSpace(List(-6, 4, -5, 8, -10, 0, 8))) // 1600
  //println(findSubArrayBruteForce(List(40, 0, -20, -10 ))) // 200
}
