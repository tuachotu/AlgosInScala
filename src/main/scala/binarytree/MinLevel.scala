package binarytree

import javax.sound.midi.MidiDevice.Info


//Given a binary tree, find its minimum depth.
//The minimum depth is the number of nodes along the shortest path from the root node down to the nearest leaf node.
object MinLevel extends  App {
  abstract class BinaryTree {
    def value: Int
    def left: BinaryTree
    def right: BinaryTree
    def isEmpty: Boolean

    def minLevel(): Int = {
      if (isEmpty) { 0 } else {
        if (left.isEmpty && right.isEmpty) 1
        else if (left.isEmpty) 1 + right.minLevel()
        else if (right.isEmpty) 1 + right.minLevel()
        else 1 + min(left.minLevel(), right.minLevel())
      }
    }
  }

  def min(i: Int, j :Int): Int = if (i < j)  i else j

  case object EmptyNode extends BinaryTree {
    override def value: Int = ???
    override def left: BinaryTree = ???
    override def right: BinaryTree = ???
    override def isEmpty: Boolean = true
  }

  case class BinaryTreeNode(value:Int, left:BinaryTree = EmptyNode, right:BinaryTree = EmptyNode) extends BinaryTree {
    override def isEmpty: Boolean = false
  }


  val inputTree1 =BinaryTreeNode(3,BinaryTreeNode(9),  BinaryTreeNode(20,BinaryTreeNode(15), BinaryTreeNode(7)))
  val inputTree2= BinaryTreeNode(2, EmptyNode, BinaryTreeNode(3, EmptyNode, BinaryTreeNode(4, EmptyNode, BinaryTreeNode( 5,EmptyNode, BinaryTreeNode(6)))))

  println(inputTree1.minLevel())
  println(inputTree2.minLevel())
}
