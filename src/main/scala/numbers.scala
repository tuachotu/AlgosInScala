import scala.collection.mutable
object numbers extends  App {
  def reverseInteger(num: Int): Int = {
    val isNegative:Boolean = if (num < 0) true else false
    val number = Math.abs(num)
    var c = number / 10
    var d = number % 10
    var str = mutable.ArrayBuffer[Char]()
    while(c > 0) {
      str :+=(d + '0').toChar
      d = c % 10
      c = c /10
    }
    str:+= (d + '0').toChar
    val i = str.indexWhere(_ != '0')
    str = str.drop(i)
    val maxValue = 2147483647
    val r = str.foldLeft(0) { (result, digit) =>
      val rr = result*10 + (digit - '0')
      if (result < (maxValue - (digit -'0'))/ 10) rr else return  0
    }
    if (isNegative) -1 * r else r



  }




  println( reverseInteger(1230000))
  println( reverseInteger(-123000100))

}
