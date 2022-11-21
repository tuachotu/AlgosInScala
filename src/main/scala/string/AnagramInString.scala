package string

object AnagramInString extends App {

  // Given two strings s and p, return an array of all the start indices of p's anagrams in s.
  // An Anagram is a word or phrase formed by rearranging the letters of a different word or phrase, typically using all the original letters exactly once.


  def findAnagrams(s:String, p: String): List[Int] = {
    def buildCharMap(ss: String): Map[Char, Int] = {
      ss.foldLeft(Map[Char, Int]()) { (charTable, c) =>
        charTable + (c -> (charTable.getOrElse(c, 0) + 1))
      }
    }

    val patCharTables = buildCharMap(p)
    (0 to s.length).foldLeft(List[Int]()) { (result ,index) =>
      val subString = s.drop(index).take(p.length)
      val subStringCharTable = buildCharMap(subString)

      if (subStringCharTable == patCharTables) index::result else result
    }
  }

  println(findAnagrams("cbaebabacd", "abc").mkString(",")) // 0,6
  println(findAnagrams("abab", "ab").mkString(",")) // 0,1,2
}
