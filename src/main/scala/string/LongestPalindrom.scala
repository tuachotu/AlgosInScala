package string

import scala.util.control.Breaks._

object LongestPalindromic extends App {

  def longestPalindromicString(s:String): String = {

     // validate here

    def logestPalindromicStringAtIndexes(l: Int, r: Int) : Int = {
      if (l > r) 0 else {
        var left = l
        var right = r
        while (left >= 0 && right < s.length && s(left) == s(right)) {
          left = left - 1
          right = right + 1
        }
        right - left + 1
      }
    }


    val r = (0 to s.length).foldLeft(0,0) { case ((resultIndex, length), index) =>
       val l1 = logestPalindromicStringAtIndexes(index, index)
      val l2 = logestPalindromicStringAtIndexes(index, index + 1 )
      val palindromLengthAtIndex = if (l1 > l2) l1 else l2
      if (length > palindromLengthAtIndex) (resultIndex, length) else (index, palindromLengthAtIndex)
    }

    println(r)
    val length = if (r._2 % 2 == 0) r._2-r._1-1 else r._2-r._1
    val start =  if (r._2 % 2 == 0) r._1-1 else r._1
    //s.drop(r._1-1).take(length )
    s.drop(r._1).take(r._2-r._1 -1 )


  }

 println(longestPalindromicString("babad"))
  println(longestPalindromicString("cbbd"))
  println(longestPalindromicString("cbbbbd"))

}
