package string

//  Input: str1 = “listen”  str2 = “silent”
//  Output: “Anagram”
//  Explanation: All characters of “listen” and “silent” are the same.
object TwoStringAnagram  extends App {
  def areAnagrams( s1: String, s2: String): Boolean = {
    (s1,s2) match {
      case _ if s1.length != s2.length => false
      case _ if s1.isEmpty && s2.isEmpty => true
      case _ =>
          val maps = (s1 zip s2).foldLeft((Map[Char, Int]()), Map[Char, Int]()) { (lookupMaps, chars) =>
            val newLookupMap1 = lookupMaps._1 + (chars._1 -> lookupMaps._1.getOrElse(chars._1, 0))
            val newLookupMap2 = lookupMaps._2 + (chars._2 -> lookupMaps._2.getOrElse(chars._2, 0))
            (newLookupMap1, newLookupMap2)
          }
        maps._1 == maps._2
    }
  }

  println(areAnagrams("listen", "silent"))
  println(areAnagrams("liste1", "silen1"))
  println(areAnagrams("l1sten", "s1lent"))
}
