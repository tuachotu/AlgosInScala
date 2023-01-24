package algo

object CountBinaryOperations extends App {
  def calNeededPow(n:Int):List[Int] = {
    var powerSer = Array.empty[Int]
    var temp = 1
    while (temp < n) {
      powerSer :+= temp
      temp = temp * 2
    }
    powerSer.toList ++ List(temp)
  }

  def countOp(num:Int): Int = {
    val powerOfTwos= calNeededPow(num)

    def countOpInternal(num:Int): Int = {
      if (num <= 0) 0
      else if (powerOfTwos.contains(num)) 1
      else {
        val nearestPowIndex = powerOfTwos.indexWhere(_ > num)
        1 + countOpInternal(num - powerOfTwos(nearestPowIndex-1))
      }
    }
    countOpInternal(num)
  }

  println(countOp(5)) // should give 2

}
