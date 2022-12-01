package graph

import scala.collection.mutable
import scala.collection.mutable.{HashSet, Stack}
// graph is made of edge
// Map [String -> List[String]


object LoopInGraph extends App {
  def detectCircle(graph: Map[String, List[String]]): Boolean = {
    if (graph.isEmpty) false else {
      var visited = mutable.HashSet[String]()
      var yetToVisit = mutable.Stack[String]()
      var  cycleFound : Boolean = false
      yetToVisit.push(graph.keys.head)

      while(yetToVisit.nonEmpty && !cycleFound) {
        val nodeToVisit = yetToVisit.pop()
        if (visited.contains(nodeToVisit)) {
          cycleFound = true
        } else {
          visited.add(nodeToVisit)
          graph(nodeToVisit) foreach (neighbor => yetToVisit.push(neighbor))
        }
      }
      cycleFound
    }
  }



val inputGraphWithCycle = Map(
  ("A" -> List("B", "E")),
  ("B" -> List()),
  ("C" -> List("B", "D")),
  ("D" -> List("A")),
  ("E" -> List("D")),
)

val inputGraphWithoutCycle = Map(
    ("A" -> List("B","D", "E")),
    ("B" -> List()),
    ("C" -> List("B", "D")),
    ("D" -> List()),
    ("E" -> List("D")),
  )


  println(detectCircle(inputGraphWithCycle))
  println(detectCircle(inputGraphWithoutCycle))


}
