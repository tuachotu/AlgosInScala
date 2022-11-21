package binarytree

object CountNodes extends App {
  abstract class BinaryTree {
    def value: Int
    def left: BinaryTree
    def right: BinaryTree
    def empty: Boolean

    def nodeCount: Int = {
      if (empty) 0 else 1 + left.nodeCount + right.nodeCount
    }
  }

  case object EmptyNode  extends BinaryTree {
    override def value: Int = ???
    override  def left: BinaryTree = ???
    override  def right: BinaryTree = ???
    override def empty: Boolean = true
  }

  case class TreeNode(value: Int, left:BinaryTree = EmptyNode, right: BinaryTree = EmptyNode) extends BinaryTree {
    override  def empty: Boolean = false
  }


  val root: TreeNode = TreeNode(1, TreeNode(2),TreeNode(3,TreeNode(4), TreeNode(5, TreeNode(6), TreeNode(7))))

  println(root.nodeCount)

}
