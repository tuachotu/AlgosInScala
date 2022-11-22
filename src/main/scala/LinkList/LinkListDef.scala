package LinkList

object LinkListDef extends App {
  abstract class LinkedList {
    def value: Int

    def next: LinkedList

    def empty: Boolean

    def add(v: Int): LinkedList = {
      if (empty) ValidNode(v)
      else ValidNode(value, next.add(v))
    }


    def reverse: LinkedList = {
      if (empty) EmptyNode
      else {
        next.reverse.add(value)
      }
    }

    def removeAll(v: Int): LinkedList = {
      if (empty) EmptyNode
      else {
        if (value == v) next.removeAll(v)
        else ValidNode(value, next.removeAll(v))
      }
    }

    def remove(v:Int): LinkedList = {
      if (empty) EmptyNode
      else {
        if (value == v) next
        else ValidNode(value, next.remove(v))
      }
    }

    def show(): Unit = {
      if (empty) {
        print("  End of List.")
      }
      else {
        print(s"$value  --> ")
        next.show()
      }
    }
  }

  case object EmptyNode extends LinkedList {
    def value: Int = ???
    def next: LinkedList = ???
    def empty: Boolean = true
  }

  case class ValidNode(value:Int, next:LinkedList = EmptyNode) extends LinkedList {
    def empty: Boolean = false
  }

  val l = Seq(1,2,3,3,3,4,5,6).foldLeft(EmptyNode.asInstanceOf[LinkedList]) { (list,value) =>
    list.add(value)
  }

  l.show()
  println()
  l.remove(3).show
  println()
  l.removeAll(3).show
  println()
  l.reverse.show



}
