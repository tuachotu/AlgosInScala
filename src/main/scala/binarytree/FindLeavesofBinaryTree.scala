package binarytree

import scala.::
import scala.collection.mutable.Stack
object FindLeavesofBinaryTree extends App {

  sealed trait BinaryTree {
    def value: Int
    def left: BinaryTree
    def right: BinaryTree
    def empty: Boolean
  }

  case object EmptyNode extends BinaryTree {
    def value: Int = ???
    def left: BinaryTree = ???
    def right: BinaryTree = ???
    def empty: Boolean = true
  }

  case class BinaryTreeNode(value: Int, left: BinaryTree = EmptyNode, right: BinaryTree = EmptyNode ) extends BinaryTree {
    def empty: Boolean = false
  }

  def findLeaves(root: BinaryTree): List[List[Int]] = {
    val lookup = Stack.empty[(BinaryTree, Int)] // root, level
    var res = Array[Array[(Int, Int)]]()
    lookup.push((root, 0))

    while (lookup.nonEmpty) {
      val (n,level) = lookup.pop
      if (n.left.empty && n.right.empty) {
        if(res.isEmpty) res = res :+ Array((n.value, level))
        else {
          if (res.last.head._2 == level)  {
            res = res.init ++ Array((n.value, level))::res.last)
          } else {
            res = res :+ Array((n.value, level))
          }
        }
      } else {

      }
    }
  }

   val input1 = BinaryTreeNode(1, BinaryTreeNode(2, BinaryTreeNode(4), BinaryTreeNode(5)), BinaryTreeNode(3))
  println(findLeaves(input1))

}
