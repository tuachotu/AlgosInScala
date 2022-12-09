package algo



// Solution
// How to get char for number = n + 'A' to char ; n will be always in limit
// run a loop on number , doing number/26 each iteration
// we need to make sure we do number-- just to take care of case 1 we get 1 as divide result

object ExcelSheetNumberToColTitle extends App {
  def getCharForNumber(n:Int): Char =  (n + 'A').toChar
  def calculateColTitle(num: Int): String = {
    var number = num
    var result: String = ""
    while(number > 0) {
      number = number - 1
      result = getCharForNumber(number % 26)+result
      number = number / 26
    }
    result
  }

  def calculateColTitleFunctional(num: Int): String = {
    if (num == 0) ""
    else {
       calculateColTitleFunctional((num-1)/26) + getCharForNumber((num-1) % 26)
    }
  }
   println(calculateColTitle(1))
  println(calculateColTitleFunctional(1))
   println(calculateColTitle(26))
  println(calculateColTitleFunctional(26))
   println(calculateColTitle(27))
  println(calculateColTitleFunctional(27))
  println(calculateColTitle(28))
  println(calculateColTitleFunctional(28))
  println(calculateColTitle(701))
  println(calculateColTitleFunctional(701))
   //1 to 100 foreach { num =>  println(num, "-->" , calculateColTitle(num)) }

}
