package binarytree

object Identical extends App {
  abstract class BinaryTree {
    def value: Int
    def left:BinaryTree
    def right: BinaryTree
    def empty: Boolean
  }

  case object EmptyNode extends BinaryTree {
    def value:Int = ???
    def left:BinaryTree = ???
    def right:BinaryTree = ???
    def empty: Boolean = true

  }

  case class Node(value:Int, left:BinaryTree= EmptyNode, right:BinaryTree = EmptyNode) extends BinaryTree {
    def empty: Boolean = false
  }

  def identicalTrees(t1: BinaryTree, t2:BinaryTree): Boolean = {
    if (t1.empty && t2.empty) true
    else if (t1.empty || t2.empty) false
    else (t1.value == t2.value) && identicalTrees(t1.left, t2.left) && identicalTrees(t1.right, t2.right)
  }


  val tree1 = Node(1, Node(2), Node(3, Node(4), Node(5)))
  val tree2 = Node(1, Node(2), Node(3, Node(4), Node(5)))
  val tree3 = Node(1, Node(2), Node(3, Node(4), Node(5, Node(6))))


  println(identicalTrees(tree1, tree3))


}
