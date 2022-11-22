package string

object NumberOfMatchingChar extends App {
  def matchingCharCount(s1: String, s2: String): Int = {
    val s1Lookup = s1.foldLeft(Map[Char, Boolean]()) { (lookup, c) =>
      if (lookup.contains(c)) lookup else lookup + (c -> true)
    }
    val s2Lookup = s2.foldLeft(s1Lookup) { (lookup, c) =>
      if (lookup.contains(c)) lookup + (c -> false) else lookup + (c -> true)
    }
    s2Lookup.count({ case (k,v) => v == false})
    //s2Lookup.count{ _._2 == false}
    //s2Lookup.values.count{ _ == false}
  }

  println(matchingCharCount("abcdef","defghia"))

}
