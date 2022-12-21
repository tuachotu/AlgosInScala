package graph

import scala.collection.mutable
// Graph is valid tree if
// its a connected graph
// it has no loop
// logic if DFT can cover every node in the graph with no loop it s a valid tree

object GraphValidTree extends App {
  def validTree(n:Int, graph: Map[Int, List[Int]]): Boolean = {
    if (graph.nonEmpty) {
      val visited =  mutable.HashSet.empty[Int]
      val pending =  mutable.Queue.empty[Int]
      var loopFound: Boolean = false
      pending.enqueue(0)
      while(pending.nonEmpty && !loopFound) {
        val node = pending.dequeue()
        if (visited.contains(node)) {
          loopFound = true
        } else {
          visited += node
          graph.getOrElse(node, Nil).foreach(p => pending.enqueue(p))
        }
      }

      if (!loopFound) visited.size == n+1 else !loopFound
    } else true
  }

  val input1 = Map( 0 -> List(1,2,3), 1-> List(4))  // tree
  val input2 = Map( 0 -> List(1), 1-> List(4,2,3), 2 -> List(3), 3 -> List(1,2)) // not a tree

  println(validTree(4, input1 ))
  println(validTree(4, input2 ))

}
