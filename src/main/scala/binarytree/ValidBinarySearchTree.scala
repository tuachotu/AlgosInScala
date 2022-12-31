package binarytree


object ValidBinarySearchTree extends App {
  sealed trait BinaryTree {
    def value: Int
    def left: BinaryTree
    def right: BinaryTree
    def empty: Boolean

    def validBST: Boolean = {
      if (empty || (left.empty && right.empty)) true
      else if (left.empty) value < right.value && right.validBST
      else if (right.empty) value >= left.value && left.validBST
      else value < right.value && value >= left.value && left.validBST && right.validBST
    }
  }
  case object EmptyNode extends BinaryTree {
    def value: Int = ???
    def left: BinaryTree  = ???
    def right: BinaryTree = ???
    def empty: Boolean = true
  }

  case class BinaryTreeNode(value: Int, left: BinaryTree = EmptyNode, right: BinaryTree = EmptyNode) extends BinaryTree {
    def empty: Boolean = false
  }

  val input1 = BinaryTreeNode(2,BinaryTreeNode(1),BinaryTreeNode(3))
  println(input1.validBST)

  val input2 = BinaryTreeNode(5, BinaryTreeNode(1), BinaryTreeNode(4, BinaryTreeNode(3), BinaryTreeNode(6)))
  println(input2.validBST)

  val input3 = BinaryTreeNode(5, BinaryTreeNode(1, EmptyNode, BinaryTreeNode(100)), BinaryTreeNode(4, BinaryTreeNode(3), BinaryTreeNode(6)))
  println(input3.validBST)

  val input4 = BinaryTreeNode(150, BinaryTreeNode(1, EmptyNode, BinaryTreeNode(100)), BinaryTreeNode(400, BinaryTreeNode(300), BinaryTreeNode(600)))
  println(input4.validBST)

}
