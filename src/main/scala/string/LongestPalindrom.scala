package string

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

object LongestPalindromic extends App {

  def longestPalindrom(s: String): String = {

    def longestPalindromicStringAtIndexes(l: Int, r: Int): String = {
      if (l > r) "" else {
        var left = l
        var right = r
        var palindrom: String = "";

        while (left >= 0 && right < s.length && s(left) == s(right)) {
          if (left == right) {
            palindrom += s(left)
          } else {
            palindrom =    s(left) + palindrom
            palindrom =   palindrom +  s(right)
          }
          left = left - 1
          right = right + 1
        }

        palindrom.mkString
      }
    }

    def findPalindromAtIndex(index : Int): String = {
      val p1 = longestPalindromicStringAtIndexes(index, index)
      val p2 = longestPalindromicStringAtIndexes(index, index+1)

      if (p1.length > p2.length) p1 else p2
    }


    def longestPalindromInternal(startFrom: Int): String = {
        if (startFrom < s.length) {
          val lngstSbStrng = findPalindromAtIndex(startFrom)
          if (lngstSbStrng.length == s.length) s
          else {
            val nextPalindrom = longestPalindromInternal(startFrom + 1)
            if (nextPalindrom.length > lngstSbStrng.length) nextPalindrom else lngstSbStrng
          }
        } else ""
    }

    if (s.isEmpty || s.filter(!_.isWhitespace).isEmpty) "" else longestPalindromInternal(0)
  }




 println(longestPalindrom("babad"))
  println(longestPalindrom("babadddd1dddd"))
  println(longestPalindrom("cbbd"))
  println(longestPalindrom("cbbbbd"))

}
