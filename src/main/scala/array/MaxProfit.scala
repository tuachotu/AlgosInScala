package array
// iterate through the list
// keep track  known minimum price and maximum profit
// if find a new minimum, use it to calculate new profit.
object MaxProfit extends App {
  def calculateMaxProfie(prices: List[Int]): Int = {
    prices.tail.foldLeft((prices.head,0)) { case ((minPrize, maxProfit), priceOnDay) =>
      val maxProfitToday = Math.max(priceOnDay - minPrize, maxProfit)
      val minPriceToday = Math.min(minPrize,priceOnDay)
      (minPriceToday, maxProfitToday)
    }._2
  }

  println(calculateMaxProfie(List(7,1,5,3,6,4)))
}
