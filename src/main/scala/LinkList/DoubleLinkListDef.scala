package LinkList

object DoubleLinkListDef extends  App {

  abstract class DoublyLinkList {
    def value: Int
    def prev: DoublyLinkList
    def next: DoublyLinkList
    def isEmpty: Boolean

    def add(v: Int): DoublyLinkList = {
      if (isEmpty) Node(v)
      else if(!next.isEmpty) {
        Node(value, prev, next.add(v))
      } else {
        val nodeToAdd = Node(v, prev = this, next = EmptyNode)
        Node(value, prev, nodeToAdd)
      }
    }

    def remove(v:Int) : DoublyLinkList = {
      if (isEmpty) this
      else if (value != v) {

        val tPrev = prev
        val rr = next.remove(v)
        Node(rr.value, tPrev, rr.next)
      } else { //found node
        if (prev.isEmpty && next.isEmpty ) EmptyNode
        else if (prev.isEmpty) next
        else {
          val newNext = next.remove(v)
          val newValue = prev.value
          Node(newValue, prev, newNext)
        }
      }

    }

    def printList():Unit = {
      if (isEmpty) print("--|") else {
        if (prev.isEmpty) print("|")
        print(s"--$value")
        next.printList()
      }
    }
  }

  case object EmptyNode extends DoublyLinkList {
    def value: Int = ???
    def prev: DoublyLinkList = ???
    def next: DoublyLinkList = ???
    def isEmpty: Boolean = true
  }

  case class Node(value:Int, prev: DoublyLinkList = EmptyNode, next:DoublyLinkList = EmptyNode ) extends DoublyLinkList {
    def isEmpty: Boolean = false
  }

  val dll = Seq(1,2,3,4,5,6,7,8).foldLeft(EmptyNode.asInstanceOf[DoublyLinkList]) { (result, value) =>
    result.add(value)
  }

  dll.printList()
  println
  //dll.remove(1).printList()

  dll.remove(5).printList()

}
