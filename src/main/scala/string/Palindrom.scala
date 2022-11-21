package string

import scala.util.control.Breaks.{break, breakable}

object Palindrom extends App {
  def isPalinrom(s: String): Boolean = {
    var left = 0
    var right = s.length - 1
    var result = false
    breakable {
      while (left <= right) {
        println(left, right)
        if (left == right) {
          result = true
          break
        }
        if (s(left) == s(right)) {
          println(s(left), s(right), left, right)
          left = left + 1
          right = right - 1
        } else {
          result = false
          break
        }
      }
    }

    result
  }

  println(isPalinrom("1112111"))
}


