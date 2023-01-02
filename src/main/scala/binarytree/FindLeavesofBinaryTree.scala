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

  def leafNodes(root: BinaryTree, knownLeaf: List[Int]): List[Int] = {
    if (root.empty) List.empty[Int]
    else if (!root.empty && knownLeaf.contains(root.value)) List.empty[Int]
    else {
      if (knownLeaf.isEmpty && root.left.empty && root.right.empty) List(root.value)
      else if (!root.left.empty && !root.right.empty && knownLeaf.contains(root.left.value) && knownLeaf.contains(root.right.value))  List(root.value) // both child visited leaves
      else if (!root.left.empty && root.right.empty && knownLeaf.contains(root.left.value))  List(root.value) // left child visited leaves
      else if (root.left.empty && !root.right.empty && knownLeaf.contains(root.right.value))  List(root.value) // right child visited leaves
      else leafNodes(root.left,knownLeaf)++leafNodes(root.right, knownLeaf)
    }
  }
  def findLeaves(root: BinaryTree): List[List[Int]] = {
    var newLeafs = leafNodes(root, List.empty[Int])
    var allFoundLeafs = newLeafs
    var res = List[List[Int]](newLeafs)
    while (newLeafs.nonEmpty) {
      println(newLeafs)
      newLeafs =  leafNodes(root, allFoundLeafs)
      if (newLeafs.nonEmpty) {
        allFoundLeafs = allFoundLeafs ++ newLeafs
        res = res++ List(newLeafs)
      }
    }
    res
  }

//   val input1 = BinaryTreeNode(1, BinaryTreeNode(2, BinaryTreeNode(4), BinaryTreeNode(5)), BinaryTreeNode(3))
//  println(findLeaves(input1))

//  val input2 = BinaryTreeNode(1, BinaryTreeNode(2, BinaryTreeNode(4), BinaryTreeNode(5, BinaryTreeNode(6))), BinaryTreeNode(3))
//  println(findLeaves(input2))


  val input3 = BinaryTreeNode(1, BinaryTreeNode(2, BinaryTreeNode(4), BinaryTreeNode(5, BinaryTreeNode(6))), BinaryTreeNode(3, BinaryTreeNode(32)))
  println(findLeaves(input3))

}
