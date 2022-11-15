package graph

import graph.GraphDefinition.{Edge, readGraph}

import scala.collection.immutable.Map
import scala.collection.mutable
import scala.collection.mutable.{HashSet, Queue, Stack}

object GraphTraversal extends  App{
  case class Edge(v1: Char,  v2:Char)

  def readGraph(edges: List[Edge]): Map[Char, List[Char]] = {
    edges match {
      case Nil => Map[Char, List[Char]]()
      case _ =>
        edges.foldLeft(Map[Char, List[Char]]()) { (graph, edge) =>
          val existingV1edges = graph.getOrElse(edge.v1, List.empty)
          val existingV2edges = graph.getOrElse(edge.v1, List.empty)

          graph + (edge.v1 -> (existingV1edges ++ List(edge.v2))) + (edge.v2 -> (existingV2edges ++ List(edge.v1)))
        }
    }
  }
  // DFT - DEPTH First Traversal
  // Will use a set to keep track of visited node
  // will use a stack to keep track of non visited node
  // Will take one char as input to start traversal from

  def depthFirstTraversal(node: Char, graph: Map[Char, List[Char]]): Unit = {

    val visitedNodes = mutable.HashSet[Char]()
    val yetToVisitNodes = mutable.Stack[Char]()

    if (graph.nonEmpty && (graph contains node)) {
      yetToVisitNodes.push(node)
      while(yetToVisitNodes.nonEmpty) {
        val nodeToVisit = yetToVisitNodes.pop()
        if (!visitedNodes.contains(nodeToVisit)) {
          visitedNodes += nodeToVisit
          graph(nodeToVisit) foreach { neighbor =>
            yetToVisitNodes.push(neighbor)
          }
          println(nodeToVisit)
        }
      }
    } else {
      println("invalid input")
    }

  }

  // BFT - Breadth First Traversal
  // Will use a set to keep track of visited node
  // will use a queue to keep track of non visited node
  // Will take one char as input to start traversal from

  def breadthFirstTraversal(node: Char, graph: Map[Char, List[Char]]): Unit = {
    val visitedNodes = mutable.HashSet[Char]();
    val pendingNodes = mutable.Queue[Char]();

    if (graph  contains node) {
      pendingNodes.enqueue(node)
      while(pendingNodes.nonEmpty) {
          val nodeToVisit = pendingNodes.dequeue()
          if (!visitedNodes.contains(nodeToVisit)) {
            visitedNodes += nodeToVisit
            graph(nodeToVisit) foreach  (pendingNodes.enqueue(_))
            println(nodeToVisit)
          }
      }
    } else {
      println("Invalid input")
    }

  }

  val myGraph = readGraph(List(Edge('s', '1'), Edge('s', '2'), Edge('1','3'), Edge('1','4'), Edge('1','5'), Edge('2','6'), Edge('6','7')))
  breadthFirstTraversal('s', myGraph)
  println("--------------------------------------------------------")
  depthFirstTraversal('s', myGraph)



}
