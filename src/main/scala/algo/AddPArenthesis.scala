package algo

object AddParenthesis extends App {
  def calculate(x:Int, y: Int, op: Char): Int = {
    op match {
      case '+' => x + y
      case '-' => x - y
      case '*' => x * y
    }
  }

  def addParenthesis(s: String): List[Int] = {
    var isNumber = true
    var res = Array.empty[Int]
    s.indices.foreach { i =>
      if (!s(i).isDigit) {
        isNumber = false
        val leftSideResults = addParenthesis(s.take(i))
        val rightSideResults = addParenthesis(s.drop(i+1))
        for {
          op1 <- leftSideResults
          op2 <- rightSideResults
        } res =  res :+ calculate(op1, op2, s(i))
      }
    }
    if (isNumber) {
      res = res :+ s.toInt
      println(res.mkString)
    }
    res.toList
  }

  println(addParenthesis("2*3-4*5"))

}
