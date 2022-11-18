package binarytree

import collection.mutable.{Queue, HashSet, Stack}
import scala.collection.mutable

object BSTLevelTraversal extends  App{

  abstract class BST {
    def value: Int
    def left: BST
    def right: BST
    def empty: Boolean
  }
  case object EmptyBSTNode extends BST {
    def value: Int = throw new NoSuchElementException("no value in EmptyNode")
    def left: BST  = throw new NoSuchElementException("no left subtree in EmptyNode")
    def right: BST = throw new NoSuchElementException("no right subtree in EmptyNode")
    def empty: Boolean = true
  }

  case class BSTNode (value: Int, left: BST = EmptyBSTNode, right: BST = EmptyBSTNode) extends BST {
    def empty: Boolean = false
  }

  def traversalByLevel(root: BST):Unit = {
    val visited = mutable.HashSet[Int]()
    val yetToVisit = mutable.Queue[BST]()

    yetToVisit.enqueue(root)

    while (yetToVisit.nonEmpty) {
      val node = yetToVisit.dequeue()
      visited.add(node.value)

      if (!node.left.empty) yetToVisit.enqueue(node.left)
      if (!node.right.empty) yetToVisit.enqueue(node.right)

      println(node.value)
    }

  }


  def traversalByLevelWithLevelNumber(root: BST):Unit = {
    val visited = mutable.HashSet[Int]()
    val yetToVisit = mutable.Queue[(BST, Int)]()

    yetToVisit.enqueue((root, 0))

    while (yetToVisit.nonEmpty) {
      val (node,level) = yetToVisit.dequeue()
      visited.add(node.value)

      if (!node.left.empty) yetToVisit.enqueue((node.left, level + 1))
      if (!node.right.empty) yetToVisit.enqueue((node.right, level + 1))

      println(s"${node.value}, level: $level")
    }

  }

  val root2 = BSTNode(3, BSTNode(9), BSTNode(20, BSTNode(15), BSTNode(7)))
  traversalByLevel(root2)
  println
  println
  println



  traversalByLevelWithLevelNumber(root2)




}
