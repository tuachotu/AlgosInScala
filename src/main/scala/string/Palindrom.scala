package string

import scala.util.control.Breaks.{break, breakable}

object Palindrom extends App {
  def isPalinrom(s: String): Boolean = {
    var left = 0
    var right = s.length - 1
    var result = false
    breakable {
      while (left < right) {
        if (s(left) == s(right)) {
          left = left + 1
          right = right - 1
          if (left >= right) {
            result = true
          }
        } else {
          result = false
          break
        }
      }
    }
    result
  }


  def isPalindromScalaWay(s:String): Boolean = {
    s match {
      case s if s.empty == true => false
      case s if s.length == 1 => true
      case s if s.length == 2 => s.head == s.last
      case s =>  (s.head == s.last) && isPalindromScalaWay(s.tail.init)
    }
  }

  println(isPalinrom("1112111"))
  println(isPalinrom("1112w111"))
  println(isPalinrom("abba"))

println
  println(isPalindromScalaWay("1112111"))
  println(isPalindromScalaWay("1112w111"))
  println(isPalindromScalaWay("abba"))
}


