package algo

/*
Example 1:

Input: words = ["wrt","wrf","er","ett","rftt"]
Output: "wertf"
 0 - w
 1 - e
 2 - r
 3 - t
 4 - f

"","","","",""
Example 2:


Input: words = ["z","x"]
Output: "zx"
Example 3:


Input: words = ["z","x","z"]
Output: ""
Explanation: The order is invalid, so return "".

go through head of input word,
prepare map and keep track of last counter
if counter mismatched


 */
object AlienDictionary extends  App {
  def findValueInMap(m: Map[Int, Char], c: Char): Option[(Int, Char)] = m.find{ case(key,value) => value == c}
  def findMaxValueInMap(m: Map[Int, Char]): Option[(Int, Char)] = if (m.nonEmpty) Some(m.maxBy(_._1)) else None

  def alienOrder(words: Array[String]): String = {
    def alienOrderForChars(charList: List[Char], mapInProgress: Map[Int, Char]): Option[Map[Int, Char]] = {
     if (charList.isEmpty) Some(mapInProgress)
     else {
        println(charList,mapInProgress )
        val ( _, mm, inv ) = charList.foldLeft((0, mapInProgress, false)) { case ((posInDir, updatedMapInProgress, invalid), c) =>
          if (!invalid) {
           println(posInDir, findValueInMap(updatedMapInProgress, c), c)
            findValueInMap(updatedMapInProgress, c) match {
              case Some(p) if (p._1 < posInDir) => (p._1, updatedMapInProgress, true ) // check this
              case Some(p) if (p._1 >= posInDir) => (p._1, updatedMapInProgress, false )
              case None =>
                val newKey = findMaxValueInMap(updatedMapInProgress) match {
                  case Some(p) => p._1 + 1
                  case _ => 0
                }
                (newKey,updatedMapInProgress + (newKey -> c), false )
            }
          } else (posInDir, updatedMapInProgress, invalid)
        }
       println(mm,inv)
       if (inv) None else Some(mm)
      }

    }

    def alienOrderInternal(wordsList: List[String], m: Map[Int, Char]): String = {

      if (wordsList.isEmpty) m.keys.mkString("") else {
        val firstChars = wordsList.map( _.head)
        val restOfWords = wordsList.map(_.tail).filter(_.nonEmpty)
        alienOrderForChars(firstChars, m) match {
          case None => ""
          case Some(updatedMap) if restOfWords.isEmpty => updatedMap.values.mkString("")
          case Some(updatedMap) => alienOrderInternal(restOfWords, updatedMap)
        }

      }
    }
    alienOrderInternal(words.toList, Map.empty[Int, Char])
  }
 println(alienOrder(Array("wrt","wrf","er","ett","rftt")))
//  println(alienOrder(Array("z", "x")))
//  println(alienOrder(Array("z", "x", "z")))
}
