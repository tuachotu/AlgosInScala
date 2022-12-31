package algo

/*
Example 1:
Input: s = "1+1"
Output: 2

Example 2:
Input: s = "6-4/2"
Output: 4

Example 3:
Input: s = "2*(5+5*2)/3+(6/2+8)"
Output: 21
 */

import scala.collection.mutable.Stack
object BasicCalculator extends App {

  def calculateSimple(s: String): Int = {
    if (s.nonEmpty) {
      s.split("-").map { subs =>
        subs.split("\\+").map { adds =>
          adds.split("\\*").map { prods =>
            prods.split("/").map(_.toInt).reduceLeft(_ / _)
          }.product
        }.sum
      }.reduceLeft(_ - _)
    } else 0
  }

//  def calculateSimpleNegativeAware(s: String): Int = {
//    if (s.nonEmpty) {
//      s.indexOf("--") match {
//        case p => s.take(p) +
//      }
//      s.split("-").map { subs =>
//        subs.split("\\+").map { adds =>
//          adds.split("\\*").map { prods =>
//            prods.split("/").map(_.toInt).reduceLeft(_ / _)
//          }.product
//        }.sum
//      }.reduceLeft(_ - _)
//    } else 0
//  }

  def splitString(s: String): List[String] = {
    (s.indexWhere(sc => sc == '(' || sc == ')') match {
      case index if index > -1 => List(s.take(index), s(index).toString) ++ splitString(s.drop(index + 1))
      case _ => List(s)
    }).filter(_.nonEmpty)
  }

  def calaculateWithParen(s: String): Int = {
    if (s.isEmpty) 0 else {
      val lookup = Stack.empty[String]
      splitString(s).foreach {
          case ")" =>
            var s = lookup.pop()
            var expr = ""
            while (s != "(") {
              expr = s + expr
              s = lookup.pop()
            }
            println(expr)
            lookup.push(calculateSimple(expr).toString)
            println(lookup)
          case c => lookup.push(c)
      }
      println(lookup.reverse.mkString(""))
      calculateSimple(lookup.reverse.mkString(""))
      }
    }





  //println(splitString("5+5*2"))
  //println(calaculateWithParen("6-(4/2)"))
//  println(calaculateWithParen("5+(6*(4/2))"))
//  println(calaculateWithParen("(5+(6*(4/2)))+2"))
  println(calaculateWithParen("(2+6*3+5-(3*14/7+2)*5)+3"))
  //println(calculateSimple("2+6*3+5-8*5"))
}
