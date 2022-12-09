package graph
import scala.collection.View.Empty
import scala.collection.mutable.HashMap
// A -> B
// B -> C, D
// C -> D
// D -> E
//
// A ->B -> C -> D -> E = 5


// A -> B
// B -> C, D
// C -> D,P
// P -> Q
// Q -> R
// R -> S
// D -> E
//
// A ->B -> C -> P -> Q -> R -> S = 5

object FindLongestChain extends App {
  case class PolicyRelation(left: Char, right: Char)
//  val lookup = HashMap[Char, Int]()
  def findLongestChain(graphInput: Map[Char, List[Char]]): Int ={
    def findLongestChainInternal(node: Char): Int =
      if (graphInput.contains(node) && graphInput(node).nonEmpty) {
        val ll = graphInput(node).foldLeft(List.empty[Int]) { (lengthsForNeighbor, neighbor) =>
          if (graphInput.contains(neighbor) && graphInput(neighbor).nonEmpty) {
            (1 + findLongestChainInternal(neighbor) :: lengthsForNeighbor)
          } else 1 :: lengthsForNeighbor
        }
        ll.max } else 0

    graphInput.keys.map(findLongestChainInternal).max + 1

  }

  def readGraph(relations: List[PolicyRelation]): Map[Char, List[Char]] = {
    relations.foldLeft(Map[Char, List[Char]]()) {(mapInProgress, c) =>
      mapInProgress + (c.left -> (c.right :: mapInProgress.getOrElse(c.left, List.empty[Char])))
    }
  }

  val input: List[PolicyRelation] =
    List(PolicyRelation('A','B'),
         PolicyRelation('B', 'C'),
         PolicyRelation('B', 'D'),
         PolicyRelation('C', 'D'),
         PolicyRelation('D', 'E'))

  val input2: List[PolicyRelation] =
    List(PolicyRelation('A', 'B'),
      PolicyRelation('B', 'C'),
      PolicyRelation('B', 'D'),
      PolicyRelation('C', 'D'),
      PolicyRelation('C', 'P'),
      PolicyRelation('P', 'Q'),
      PolicyRelation('Q', 'R'),
      PolicyRelation('R', 'S'),
      PolicyRelation('D', 'E'))

  println(findLongestChain( readGraph(input))) // 5
  println(findLongestChain( readGraph(input2))) // 7

}
