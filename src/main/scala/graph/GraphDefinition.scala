package graph

import scala.collection.immutable.Map
import scala.collection.mutable.{ArrayBuffer, HashMap}
object GraphDefinition extends App {
  case class Edge(v1: Char, v2: Char)

  // Using Mutables
  def readGraph(inputEdges: List[Edge]): HashMap[Char, ArrayBuffer[Char]] = {
    inputEdges match {
      case Nil => HashMap[Char, ArrayBuffer[Char]]()
      case _ => 
        inputEdges.foldLeft(HashMap[Char, ArrayBuffer[Char]]()) { (adjList, edge) =>
          if (adjList contains edge.v1) adjList(edge.v1) += edge.v2 else adjList += (edge.v1 -> ArrayBuffer(edge.v2))
          if (adjList contains edge.v2) adjList(edge.v2) += edge.v1 else adjList += (edge.v2 -> ArrayBuffer(edge.v1))
          adjList
        }
    }
  }


  // Using Immutables data structures
  def readGraphImmutable1(inputEdges: List[Edge]): Map[Char, List[Char]] = {
    inputEdges match {
      case Nil => Map[Char, List[Char]]()
      case _ =>
        inputEdges.foldLeft(Map[Char, List[Char]]()) { (adjList, edge) =>
          val adjListWithV1 = if (adjList contains edge.v1) adjList + (edge.v1 -> (adjList(edge.v1) ++ List(edge.v2))) else adjList + (edge.v1 -> List(edge.v2))
          val adjListWithV1andV2 = if (adjListWithV1 contains edge.v2) adjListWithV1 + (edge.v2 -> (adjListWithV1(edge.v2) ++ List(edge.v1))) else adjListWithV1 + (edge.v2 -> List(edge.v1))
          adjListWithV1andV2
        }
    }
  }

  def readGraphImmutable2(inputEdges: List[Edge]): Map[Char, List[Char]] = {
    inputEdges match {
      case Nil => Map[Char, List[Char]]()
      case _ =>
        inputEdges.foldLeft(Map[Char, List[Char]]()) { (adjList, edge) =>
          val v1Items = adjList.getOrElse(edge.v1, List.empty)
          val v2Items = adjList.getOrElse(edge.v2, List.empty)
          adjList + (edge.v1 -> (v1Items ++ List(edge.v2))) + (edge.v2 -> (v2Items ++ List(edge.v1)))
        }
    }
  }

  def drawGraph(graph: HashMap[Char, ArrayBuffer[Char]]): Unit  = {
    graph foreach { case (node, edges) => println(s"$node -> ${edges.mkString(",")}")}
  }
  def drawImmutableGraph(graph: Map[Char, List[Char]]): Unit  = {
    graph foreach { case (node, edges) => println(s"$node -> ${edges.mkString(",")}")}
  }

  val myGraphMutable = readGraph(List(Edge('s', '1'), Edge('s', '2'), Edge('1','3'), Edge('1','4'), Edge('1','5'), Edge('2','6'), Edge('6','7')))
  drawGraph(myGraphMutable)
  println("--------------------------------------------------------")

  val myGraphImMutable = readGraphImmutable2(List(Edge('s', '1'), Edge('s', '2'), Edge('1','3'), Edge('1','4'), Edge('1','5'), Edge('2','6'), Edge('6','7')))
  drawImmutableGraph(myGraphImMutable)



}
