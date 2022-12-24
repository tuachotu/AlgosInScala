package algo

object StockPriceFluctuation extends App {

  case class StockPrice(timeStamp: Int, price: Int) extends Ordered[StockPrice] {
    override def compare(that: StockPrice): Int = this.price.compareTo(that.price)
  }

  case class StockPriceStore(records: Map[Int, Int] = Map.empty[Int, Int] ,currentMax: Int = 0, currentMin: Int = 0 ) {
    def update(p : StockPrice): StockPriceStore =  {
      val newMax = Math.max(currentMax, p.price)
      val newMin = if (currentMin == 0) p.price else Math.min(currentMin, p.price)

      StockPriceStore(records + (p.timeStamp -> p.price), newMax, newMin)
    }
    def current: Int = records(records.keys.toList.max)
    def maxPrice: Int =  currentMax
    def minimum: Int = currentMin
  }




}
