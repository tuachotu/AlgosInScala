package graph

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer



object GraphBFTAlgos extends App {

  def bft(start: Char , graph: Map[Char, List[Char]]): List[Char] = {
    if (graph.contains(start)) {
      val visited =  mutable.HashSet.empty[Char]
      val pendingNodes = mutable.Queue[Char](start)
      val result = mutable.ArrayBuffer.empty[Char]
      while(pendingNodes.nonEmpty) {
        val node = pendingNodes.dequeue()
        if (!visited.contains(node)) {
          visited += node
          graph.getOrElse(node, List.empty[Char]) foreach ( c => pendingNodes.enqueue(c))
          result += node
        }
      }
      result.toList
    } else List.empty[Char]
  }

  def bftNonConnected(start: Char, graph: Map[Char, List[Char]]): List[Char] = {
    if (graph.contains(start)) {
      val keyToCheck = graph.keys.to(ArrayBuffer)
      val visited = mutable.HashSet.empty[Char]
      val pendingNodes = mutable.Queue[Char](start)
      val result = mutable.ArrayBuffer.empty[Char]
      while (keyToCheck.nonEmpty) {
          //println(keyToCheck,"vikrant")
          while (pendingNodes.nonEmpty) {
          val node = pendingNodes.dequeue()
          if (!visited.contains(node)) {
            visited += node
            keyToCheck -= node
            graph.getOrElse(node, List.empty[Char]) foreach (c => pendingNodes.enqueue(c))
            result += node
          }
        }
        if (pendingNodes.isEmpty && keyToCheck.nonEmpty) {
          pendingNodes.enqueue(keyToCheck.head)
          keyToCheck.remove(0)
        }
      }
      result.toList
    } else List.empty[Char]
  }

  //  val input1 = Map('0' -> List('1','2'), '1' -> List('2'), '2' -> List('0', '3'), '3' -> List('3'))
//  println(bft('2', input1))

  val input2 = Map('0' -> List('1','2'), '1' -> List('2'), '2' -> List('0', '3'), '3' -> List('3'), 'a' -> List('b', 'c'), 'c' -> List('b'))
  //println(bftNonConnected('a', input2))
  println(bftNonConnected('c', input2))
}
