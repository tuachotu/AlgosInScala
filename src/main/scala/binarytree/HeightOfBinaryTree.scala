package binarytree

object HeightOfBinaryTree extends App {
  abstract class BinaryTree extends App {
    def value: Int
    def left: BinaryTree
    def right: BinaryTree
    def empty: Boolean

    def depth():Int = {
      if (empty) 0
      else if (left.empty && right.empty) 1
      else 1 + Math.max(left.depth(), right.depth())
    }
  }

  case object EmptyNode extends BinaryTree {
    def value: Int = ???
    def left: BinaryTree = ???
    def right: BinaryTree = ???
    def empty: Boolean = true
  }

  case class BinaryTreeNode(value: Int, left: BinaryTree = EmptyNode, right: BinaryTree = EmptyNode) extends BinaryTree {
    def empty: Boolean = false
  }

  //val inputTree =   BinaryTreeNode(3, BinaryTreeNode(2,BinaryTreeNode(1), EmptyNode), BinaryTreeNode(5, BinaryTreeNode(4), BinaryTreeNode(6,EmptyNode, BinaryTreeNode(7))))
  val inputTree =   BinaryTreeNode(3, BinaryTreeNode(2,BinaryTreeNode(1), BinaryTreeNode(12)), BinaryTreeNode(5, BinaryTreeNode(4), BinaryTreeNode(6)))

  println(inputTree.depth() - 1)


}
