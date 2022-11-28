package LinkList

object RemoveNthElement extends App {
  abstract class LinkedList {
    def value: Int
    def next: LinkedList
    def empty: Boolean

    def printLinkList(): Unit = {
      if (!empty) {
        print(s" $value -->")
        next.printLinkList()
      } else {
        print("End")
      }
    }

    def removeNthNode(n:Int): LinkedList = {
      n match {
        case _ if empty => this
        case 1 => next
        case 2 if next.empty => this
        case 2 => LinkedListNode(value,next.next)
        case _ => LinkedListNode(value, next.removeNthNode(n-1))
      }

    }
  }

  case object EmptyLinkedListNode extends  LinkedList {
    def value: Int = ???
    def next: LinkedList = ???
    def empty: Boolean = true
  }

  case class LinkedListNode( value:Int, next:LinkedList = EmptyLinkedListNode) extends LinkedList {
    def empty: Boolean = false
  }


  val input = LinkedListNode(1, LinkedListNode(2, LinkedListNode(3, LinkedListNode(4, LinkedListNode(5)))))

  input.printLinkList()
  println
  input.removeNthNode(4).printLinkList()
  println
  input.removeNthNode(1).printLinkList()
  println
  input.removeNthNode(2).printLinkList()

}
