package string


import scala.collection.mutable.{HashMap, ListBuffer}
object LongestUniqueSubString extends App {

  def lengthOfLongestUniqueSubstring(s: String): Int = {
    def lengthOfLongestUniqueSubstringInternal(startFrom: Int): Int = {
      val stringToWorkOn = s.drop(startFrom)
      stringToWorkOn match {
        case _ if stringToWorkOn.isEmpty => 0
        case _ =>
          val lngstSbStrng = ListBuffer[Char]()
          stringToWorkOn.takeWhile { c =>
            if (lngstSbStrng.contains(c)) {
              false
            } else {
              lngstSbStrng+=c
              true
            }
          }
          if (lngstSbStrng.length == s.length) s.length
          else Math.max(lngstSbStrng.length, lengthOfLongestUniqueSubstringInternal(startFrom + lngstSbStrng.length))
      }
    }

    if (s.isEmpty || s.filter(!_.isWhitespace).isEmpty) 0 else lengthOfLongestUniqueSubstringInternal(0)
  }

  def lengthOfLongestSubstring(s: String): Int = {
    if (s.isEmpty || s.filter(!_.isWhitespace).isEmpty) 0
    else {
      val lookupTable = HashMap[Char, Int]()
      var start = 0
      var length = 0
      for ((c, i) <- s.zipWithIndex) {
        if (!(lookupTable contains c)) {
          lookupTable(c) = i
        } else {
          val newLength = i - start
          if (newLength > length) length = newLength
          start = lookupTable(c) + 1
          lookupTable.retain((k, v) => v >= start)
          lookupTable(c) = i
        }
      }
      Math.max(length, (s.length - start))
    }
  }

  Seq("abcabcbb", "pwwkew", "", "nnnn", "b", "  ", "aab", "abca", "abba") foreach { s =>
    println(s + " = " + lengthOfLongestSubstring(s), lengthOfLongestUniqueSubstring(s))
  }
}
