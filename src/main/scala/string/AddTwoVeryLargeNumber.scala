package string

object AddTwoVeryLargeNumber extends App {

  /*
   Solution -
    - If both numbers are empty , empty is result
    - If either of number is empty other number is result
    - Pick the bigger length and iterate over both numbers reverse
    - Get a list of two number - (n1,n2)
    - go through this list, via a fold left and and create result
   */
  def addTwoLargeNumbers(num1: String, num2: String): String = {
    (num1, num2) match {
      case _ if num1.isEmpty && num2.isEmpty => ""
      case _ if num1.isEmpty  => num2
      case _ if  num2.isEmpty => num1
      case _ =>
        val num1Reversed = num1.reverse
        val num2Reversed = num2.reverse
        val num1Length = num1.length
        val num2Length = num2.length

        val resultSize = Math.max(num1Length,num2Length)

        val numbers = for {
            i <- 0 until resultSize
            number1 = if (i < num1Length) num1Reversed(i) - '0' else 0
            number2 = if (i < num2Length) num2Reversed(i) - '0' else 0
        } yield (number1, number2)

        val (result, carry) = numbers.foldLeft(("", 0)) { case ((result, carry), pair) =>
          val sum = pair._1 + pair._2 + carry
          val resultTillNow = ((sum % 10) + '0').toChar.toString + result
          val newCarry = sum/10

          (resultTillNow , newCarry)
        }

        if (carry == 0) result else "1"+ result

    }

  }

  println(addTwoLargeNumbers("99999999999", "99999999999"))
  println(addTwoLargeNumbers("123496789", "123456789"))

}
