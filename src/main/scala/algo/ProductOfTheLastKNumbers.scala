package algo

object ProductOfTheLastKNumbers extends App {

  object ProductOfNumbers {
    def apply(): ProductOfNumbers = new ProductOfNumbers()
  }

  class ProductOfNumbers (var data :Array[Int] =  Array.empty[Int]) {

    def add(num: Int): Unit = {
        data = data :+ num
    }

    def getProduct(k: Int): Int = {
      data.reverse.take(k).product
    }
  }

  val input = ProductOfNumbers()
  input.add(3)
  input.add(0)
  input.add(2)
  input.add(5)
  input.add(4)
  println(input.getProduct(2))
  println(input.getProduct(3))
  println(input.getProduct(4))
  input.add(8)
  println(input.getProduct(2))
}
