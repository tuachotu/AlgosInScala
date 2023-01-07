package LinkList



object BinaryTreeToLinkList extends App {

  class ListNode(var value: Int, var nextNode: Option[ListNode] = None)

  class LinkList {
    var startOfList: Option[ListNode] = None

    def append(v: Int) : Unit = {
      startOfList match {
        case None =>
          startOfList = Some(new ListNode(v))
        case Some(n) =>
          var temp = n
          while(temp.nextNode.isDefined) temp = temp.nextNode.get
          temp.nextNode = Some(new ListNode(v))
      }
    }

    def show: Unit = {
      startOfList match {
        case Some(node) =>
          print("Start")
          print("--->")
          var temp = startOfList
          while (temp.isDefined) {
            print(temp.get.value)
            temp = temp.get.nextNode
            if (temp.isDefined) print("-->")
          }
          print("--|")
        case _ => print("")

      }
    }
  }

  sealed trait BinaryTree {
    def value: Int
    def left: BinaryTree
    def right: BinaryTree
    def empty: Boolean
    def appendToRightEnd(t:BinaryTree): BinaryTree = {
      if (empty) t
      else BinaryTreeNode(value, left, right.appendToRightEnd(t))
    }

    def inOrder: List[Int] = {
      if (empty) List.empty[Int]
      else left.inOrder ++ List(value) ++ right.inOrder
    }

    def preOrder: List[Int] = {
      if (empty) List.empty[Int]
      else  List(value)++ left.preOrder ++ right.preOrder
    }

    def preOrderFlatten: BinaryTree = {
      if (empty) EmptyTreeNode else {
        val leftFlatten = left.preOrderFlatten
        val rightFlatten = right.preOrderFlatten
        val combinedLeftAndRight = leftFlatten.appendToRightEnd(rightFlatten)
        BinaryTreeNode(value, EmptyTreeNode, combinedLeftAndRight)
      }


    }
  }

  case object EmptyTreeNode extends BinaryTree {
    def value: Int = ???
    def left: BinaryTree = ???
    def right: BinaryTree = ???
    def empty: Boolean = true
  }

  case class BinaryTreeNode(value: Int, left: BinaryTree = EmptyTreeNode, right: BinaryTree = EmptyTreeNode) extends BinaryTree {
    def empty: Boolean = false
  }

  val input = BinaryTreeNode(1, BinaryTreeNode(2, BinaryTreeNode(3), BinaryTreeNode(4)), BinaryTreeNode(5, right=BinaryTreeNode(6)))

  val res = new LinkList
  input.preOrder foreach res.append
  println(input)
  println(input.preOrderFlatten)
  res.show

}
