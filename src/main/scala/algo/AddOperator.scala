package algo

object AddOperator extends App {
  def addOperator2(nums: List[String]): List[String] = {
      nums match {
        case Nil => List.empty[String]
        case _::Nil => nums
        case left::tail =>
          val rightSideOps = addOperator2(tail)
          rightSideOps.flatMap(expr => List(left + "+" + expr, left + "-" + expr,left + "*" + expr))
      }
  }

  //addOperator(List(1,2,3).map(_.toString)) foreach println
  addOperator2(List(1,2,3).map(_.toString)) foreach println
  //addOperator2(List(1).map(_.toString)) foreach println

}
