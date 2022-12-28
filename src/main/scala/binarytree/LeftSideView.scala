package binarytree

object LeftSideView extends  App {
  abstract class TreeNode {
    def value: Int
    def left: TreeNode
    def right: TreeNode
    def empty: Boolean
  }

  case object EmptyTreeNode extends TreeNode {
    def value: Int = ???
    def left: TreeNode = ???
    def right: TreeNode = ???
    def empty: Boolean = true
  }


  case class ValidTreeNode(value: Int, left: TreeNode = EmptyTreeNode, right: TreeNode = EmptyTreeNode) extends TreeNode {
    def empty: Boolean = false
  }

  def LeftSideView(node: TreeNode): List[Int] = {
    val yetToVisit = scala.collection.mutable.Queue[(TreeNode, Int)]((node, 0))
    val traversalResult = scala.collection.mutable.Queue.empty[Int]
    var lastLevel = 0
    var lastValue = node.value
    while (yetToVisit.nonEmpty) {
      val (node, level) = yetToVisit.dequeue()
      if (!node.right.empty) yetToVisit.enqueue((node.right, level + 1))
      if (!node.left.empty) yetToVisit.enqueue((node.left, level + 1))


      if (level != lastLevel)
        traversalResult += lastValue
      if (yetToVisit.isEmpty)
        traversalResult += node.value
      else {
        lastValue = node.value
        lastLevel = level
      }
    }
    traversalResult.toList
  }

  val input1 = ValidTreeNode(1, ValidTreeNode(2),
    ValidTreeNode(3, right = ValidTreeNode(6, right = ValidTreeNode(8))))

  println(LeftSideView(input1))

  val input2 = ValidTreeNode(1, ValidTreeNode(2, ValidTreeNode(4, ValidTreeNode(7)), ValidTreeNode(5)),
    ValidTreeNode(3, right = ValidTreeNode(6, right = ValidTreeNode(8))))

  println(LeftSideView(input2))

}

