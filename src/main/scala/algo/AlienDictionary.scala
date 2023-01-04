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
  def buildGraphFromWords(words: List[String]): Map[Char, List[Char]] = {
    words.sliding(2).foldLeft(Map.empty[Char, List[Char]]) { case (graph, List(w1, w2)) =>
      w1.zip(w2).toList.indexWhere{ case (c1,c2) => c1 != c2 } match {
        case -1 => graph
        case index => graph + (w1(index) -> (w2(index)::graph.getOrElse(w1(index), List.empty[Char])))
      }
    }
  }

  //do DFT traversal and take care of not connected components
  def buildOrder(graph: Map[Char, List[Char]]): Option[String] = {
    var keysSuperSet = graph.keys.toList
    var loopFound = false
    var order = scala.collection.mutable.ListBuffer.empty[Char]
    while (keysSuperSet.nonEmpty && !loopFound) {
      val pendingKeys = scala.collection.mutable.Stack.empty[Char]
      val visitedKeys = scala.collection.mutable.ListBuffer.empty[Char]
      pendingKeys.push(keysSuperSet.head)
      while (pendingKeys.nonEmpty && !loopFound) {
        val keyToVisit = pendingKeys.pop()
        if (visitedKeys.contains(keyToVisit)) {
          loopFound = true
        } else {
          visitedKeys += keyToVisit
          if (graph.contains(keyToVisit)) { graph(keyToVisit) foreach { k => if (!order.contains(k)) pendingKeys.push(k)}}
        }
      }
      if (!loopFound) {
        order = order ++ visitedKeys
        keysSuperSet = keysSuperSet.filter( t => !visitedKeys.contains(t) )
      }
    }
    if (loopFound) None else Some(order.mkString(""))
  }

  def alienOrder(words: Array[String]): String = {
    buildOrder(buildGraphFromWords(words.toList)).getOrElse("")
  }
 println(alienOrder(Array("wrt","wrf","er","ett","rftt")))
 println(alienOrder(Array("z", "x")))
  println(alienOrder(Array("z", "x", "z")))
}
