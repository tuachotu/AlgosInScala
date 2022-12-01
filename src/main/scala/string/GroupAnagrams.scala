package string

// Given a list of string group all anagrams togehter
object GroupAnagrams extends App {
  def buildCharMap(s: String) = s.foldLeft(Map[Char, Int]()) { (m,c) =>
    m + (c -> (m.getOrElse(c, 0) + 1))
  }

  def groupAnagrams(names: List[String]): List[List[String]] = {
    val result = names.foldLeft(Map[String, Map[Char, Int]]()) { case (nameCharMaps, name) =>
      val charMapForName = buildCharMap(name)
      val nameWithSameCharMap = nameCharMaps.find { case (k,v) => v == charMapForName} match {
        case Some(name) => name._1
        case _ => ""
      }
      if (nameWithSameCharMap == "")  {
        nameCharMaps + (name -> charMapForName)
      } else {
        nameCharMaps - nameWithSameCharMap + ((name + ","+ nameWithSameCharMap) -> charMapForName)
      }
    }

    result.keys.toList map {key => key.split(',').toList}
 }

  println( groupAnagrams(List("abc", "xyz", "pqr", "cba", "bac", "yzx")).mkString(","))
}
