package binarytree

object BinaryTreeAllLeavesLeftToRight extends App {
  abstract class BinaryTree {
    def value: Int
    def left: BinaryTree
    def right: BinaryTree
    def empty: Boolean

    def leafLeftToRight: Unit = {
      if(!empty) {
        if (left.empty && right.empty) print(s"$value ") else {
          if (!left.empty) left.leafLeftToRight
          if (!right.empty) right.leafLeftToRight
        }
      }
    }
  }

  case object EmptyNode extends BinaryTree {
    def value: Int = ???
    def left: BinaryTree = ???
    def right: BinaryTree = ???
    def empty: Boolean = true
  }

  case class BinaryTreeNode(value: Int, left: BinaryTree = EmptyNode, right:BinaryTree = EmptyNode) extends BinaryTree {
    def empty: Boolean = false
  }


  val root = BinaryTreeNode(1,
                BinaryTreeNode(2,BinaryTreeNode(4)),
                BinaryTreeNode(3,
                    BinaryTreeNode(5, BinaryTreeNode(6), BinaryTreeNode(7)),
                    BinaryTreeNode(8, BinaryTreeNode(9), BinaryTreeNode(10))))

  root.leafLeftToRight

}
