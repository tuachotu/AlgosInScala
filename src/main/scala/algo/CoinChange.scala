package algo

object CoinChange extends App {

  /*
  [1,2,5] 11

  1 - 10
  2 - 9
  5 - 6


   */
  val lookup = scala.collection.mutable.HashMap.empty[Int,Int]
  def coinChange(coins: Array[Int], amount: Int): Int = {
    if (lookup.contains(amount))  { lookup(amount) } else {
      val res = amount match {
        case 0 => 0
        case _ if amount < 0 => -1
        case _ =>
          val firstPass = coins.map(amount - _)

          if (firstPass.contains(0)) {
            1
          } else {
            firstPass.filter(_ > 0).toList match {
              case Nil => -1
              case ll =>
                ll.map(pendingAmount => coinChange(coins, pendingAmount)).min match {
                  case -1 => -1
                  case t => 1 + t
                }
            }
          }
      }
      lookup += (amount -> res)
      res
    }

  }
//  println(coinChange(Array(1,2,5), 100))
////  println(coinChange(Array(1,2,5), 11))
  println(coinChange(Array(2), 3))
//  println(coinChange(Array(1), 0))


}
