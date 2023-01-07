package LinkList

object BinaryTreeToDoublyLinkList extends App {
  class DListNode(var value: Int, var prev: Option[DListNode] = None, var next: Option[DListNode] = None)

  class DList {
    var head: Option[DListNode] = None
    var last: Option[DListNode] = None

    def prepend(v: Int): Unit = {
      head match {
        case Some(n) =>
          val newNode = new DListNode(v, prev = None, next = head)
          head = Some(newNode)
        case None =>
          val temp = new DListNode(v)
          head = Some(temp)
          last = Some(temp)
      }
    }

    def append(v: Int) = {
      head match {
        case Some(n) =>
          var temp: Option[DListNode] = head
          var tempParent: Option[DListNode] = head
          while (temp.isDefined) {
            tempParent = temp
            temp = temp.get.next
          }
          val newNode = new DListNode(v)
          tempParent.get.next = Some(newNode)
          last = Some(newNode)

        case None =>
          val temp = new DListNode(v)
          head = Some(temp)
          last = Some(temp)
      }

    }

    def show: Unit = {
      head match {
        case Some(v) =>
          print("head -->")
          var temp = head
          while (temp.isDefined) {
            print(temp.get.value)
            temp = temp.get.next
            if (temp.isDefined) print("<-->")
          }
          print("<-- last")
        case None => print("")
      }
      println()
    }

  }

  sealed trait BinaryTree {
    def value: Int
    def left : BinaryTree
    def right: BinaryTree
    def empty: Boolean
    def inOrder: List[Int] = {
      if (empty) List.empty[Int]
      else left.inOrder ++ List(value)++ right.inOrder
    }
  }

  case object EmptyTreeNode extends BinaryTree {
    def value: Int = ???
    def left: BinaryTree = ???
    def right: BinaryTree = ???
    def empty: Boolean = true
  }

  case class BinaryTreeNode( value: Int, left: BinaryTree = EmptyTreeNode, right: BinaryTree = EmptyTreeNode) extends BinaryTree {
    def empty: Boolean = false
  }



  /*
            5
          /    \
          4     6
         /  \  /  \
        1   2  3  7

        INORDER = 1425367


   */
  val input = BinaryTreeNode(5, BinaryTreeNode(4, BinaryTreeNode(1), BinaryTreeNode(2)), BinaryTreeNode(6, BinaryTreeNode(3), BinaryTreeNode(7)))

  var res: DList = new DList

  input.inOrder.foreach(res.append)
  res.show
  

}
