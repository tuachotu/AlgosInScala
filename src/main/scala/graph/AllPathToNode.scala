package graph

import scala.collection.mutable
import scala.collection.mutable._
//Given a directed acyclic graph (DAG) of n nodes labeled from 0 to n - 1,
// find all possible paths from node 0 to node n - 1 and return them in any order.


// SOlution-
// figure out source and end of the path, in this case it is smallest key and largest key
// unique path need to be build as we progress in DFS (using recursion)
// first call (List(start), start, target)
//             if there are no path going out from start, return List[List[Int]]()
//             else iterate through each neighbor and collect the path in List[List[Int]]()
// Calls to neighbor
//            r1 = if neighbor is target, we found one path (i.e. List(start, neighbor)  - List[Int]
//            r2 = if neighbor is not, call (List(start, neighbor), neighbor, target) - List[List[Int])
//            return r1::r2

object AllPathToNode extends App {
  def findAllPath2(graph: Map[Int, List[Int]]): List[List[Int]] = {
    def findPathInternal2(pathInProgress: List[Int], node: Int, target: Int): List[List[Int]] = {
      if (graph(node).isEmpty) List[List[Int]]()
      else {
        // flat map convert  List(List(List(1)), List(List(2))) => List(List(1), List(2))
        graph(node).flatMap { neighbor =>
          if (neighbor == target) List(pathInProgress ++ List(target))
          else findPathInternal2(pathInProgress ++ List(neighbor), neighbor, target)
        }
      }
    }

    val start = graph.keys.toList.sorted.head
    val end = graph.keys.toList.sorted.last
    findPathInternal2(List(start), start, end).filter(_.nonEmpty)
  }


  val input1 = Map (
    (0 -> List(1,2)),
    (1 -> List(3)),
    (2 -> List(3)),
    (3 -> List())
  )

  val input2 = Map (
    (0 -> List(4,3,1)),
    (1 ->List(3,2,4)),
    (2-> List(3)),
    (3->List(4)),
    (4->List())
  )


  findAllPath2(input1) foreach { path =>
    println(path.mkString("->"))
  }
println("================")
  println("================")
  findAllPath2(input2) foreach { path =>
    println(path.mkString("->"))
  }
}
