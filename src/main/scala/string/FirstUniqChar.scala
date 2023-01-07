package string

object FirstUniqChar extends App {
  def firstUniqueChar(s: String): Int = {
    val lookup = (0 until s.length).foldLeft(Map[Char, (Int, Boolean)]()) { case (lookup, index) =>
      if (lookup.contains(s(index))) lookup + (s(index) -> (index, false))
      else lookup + (s(index) -> (index, true))
    }

    lookup.values.filter(_._2 == true).map(_._1).min
  }

  println(firstUniqueChar("leetcode"))
  println(firstUniqueChar("loveleetcode"))

}
