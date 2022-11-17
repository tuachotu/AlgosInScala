package binarytree

abstract class BinaryTree {
  def value: Int // What you store in the tree
  def left: BinaryTree
  def right: BinaryTree
  def isEmpty: Boolean

  def add(v: Int): BinaryTree = {
    if (isEmpty) BinaryTreeNode(v)
    else if (v == value) this // same tree
    else if (v < value) BinaryTreeNode(value, left.add(v), right)
    else BinaryTreeNode(value, left, right.add(v))
  }
  def min():Int = {
    def findMin(t: BinaryTree, v: Int):Int = if (t.isEmpty) v else findMin(t.left, t.value)
    if (isEmpty) throw new NoSuchElementException("no minimum in Empty tree")
    else findMin(left, value)
  }

  def remove(v: Int): BinaryTree = {
    if (isEmpty) throw new NoSuchElementException("remove called on Empty Tree")
    else if ((v < value && left.isEmpty) || (v > value && right.isEmpty))  this
    else if (v < value && !left.isEmpty)  BinaryTreeNode(value, left.remove(v), right)
    else if (v > value)  BinaryTreeNode(value, left, right.remove(v))
    else  { //  (v == value)
      // if both left and right is empty , return empty tree (i.e. discard current node)
      if (left.isEmpty && right.isEmpty) EmptyNode
      else if (left.isEmpty) right
      else if (right.isEmpty) left
      else { // have a valid left and right tree, find min element in right and replace it
        val replacement = left.min()
        BinaryTreeNode(replacement, left, right.remove(replacement))
      }
      // find the minimum of left sub tree
    }

  }


}

case object EmptyNode extends BinaryTree {
  def value: Int = throw new NoSuchElementException("Empty node , no value exist")
  def left: BinaryTree = throw new NoSuchElementException("Empty node , no left tree exist")
  def right: BinaryTree = throw new NoSuchElementException("Empty node , no right tree exist")
  override def isEmpty: Boolean = true
}

case class BinaryTreeNode(value: Int, left: BinaryTree = EmptyNode, right: BinaryTree = EmptyNode) extends BinaryTree {
  override def isEmpty: Boolean = false
}


object BinaryTreeDefinition extends App {
println("Hello World")
}
