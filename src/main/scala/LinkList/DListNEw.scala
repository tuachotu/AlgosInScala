package LinkList

object DListNew extends App{
  class DListNode(var value: Int, var prev: Option[DListNode] = None, var next: Option[DListNode] = None )

  class DList
  {
    var head: Option[DListNode] = None
    var last: Option[DListNode] = None

    def prepend(v:Int): Unit = {
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
        case None =>  print("")
      }
      println()
    }

  }

  val input1 = new DList
  input1.show
  input1.append(1)
  input1.append(2)
  input1.show

  for (i <- 3 to 15) input1.append(i)

  input1.show

  input1.prepend(0)

  input1.show

  for (i <- -1 to -10 by -1) input1.prepend(i)

  input1.show

}
