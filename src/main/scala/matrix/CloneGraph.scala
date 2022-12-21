package matrix

import scala.collection.mutable

object GraphPlayGround extends App {
    abstract class Node {
      def value: Int
      def neighbors: List[Node]
      def empty: Boolean
      def printGraph: Unit = { // print everything
        if (!empty) {
          print(s"$value -->")
          neighbors.foreach(_.printGraph)
        }
      }
    }

    case object EmptyNode extends Node {
      def value: Int = ???
      def neighbors: List[Node] = ???
      def empty: Boolean = true
    }

    case class GraphNode(value: Int, neighbors: List[Node] = Nil) extends  Node  {
      def empty: Boolean = false
    }

    def dft(start: Node): Unit = {
      if (!start.empty) {
        val visited = mutable.HashSet.empty[Int]
        val pending = mutable.Queue.empty[Node]
        pending.enqueue(start)
        while(pending.nonEmpty) {
          val n = pending.dequeue()
          if (!visited.contains(n.value)) {
            visited.add(n.value)
            n.neighbors foreach(neighbor => pending.enqueue(neighbor))
            print(s"${n.value} ,")
          }
        }
      }
    }


  def clone(startNode: Node): Node = {
    if (startNode.empty) EmptyNode else {
      val neighborsCopy = startNode.neighbors.map(n => clone(n))
      GraphNode(startNode.value, neighborsCopy)
    }
  }

    val input1= GraphNode(1, List(GraphNode(2, List(GraphNode(3))), GraphNode(4, List(GraphNode(3)))))
//    input1.printGraph
//    println()
//    dft(input1)
    val input1Clone = clone(input1)
    input1.printGraph
    println()
    input1Clone.printGraph
    println()
    dft(input1)
    println()
    dft(input1Clone)
//  def cloneGraph(graphNode: Node): Node = {
//
//  }

}
