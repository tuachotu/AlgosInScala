package algo

/*
req
 -               /
              a       b
          a   c  b    a b c


 level -> nodes
  0 - /
  1 -a,b
  2

  Node {
  key: String
  info: String
  childs: List[String]
  }

  /a/b/c/d

 */
object folderMgmt extends App {
  class FolderNode(var key:String, var value: String, var children:List[FolderNode] = List.empty[FolderNode] ) {
    def processAPath(components: List[String], value: String): Boolean = {
      components match {
        case Nil => false
        case head::Nil =>
          if (children.exists(child => child.key == head)) false
          else {
            val newNode = new FolderNode(head, value)
            children = newNode::children
            true
          }
        case head::tail =>
          children.indexWhere(child => child.key == head) match {
            case -1 => false
            case index => children(index).processAPath(tail, value)
          }
      }
    }

    def printFolders(padding: String= ""): Unit = {
      println(s"$padding ($key, $value)")
      if (children.nonEmpty)  {
        children foreach( child => child.printFolders( padding + "    "))
      }
    }


    def createPath(path: String, value: String): Unit = {
      if (!processAPath(path.split('/').toList.tail, value)) throw new RuntimeException("Bad Input")
    }

    def findAValueInternal(components: List[String]): String = {
      components match {
        case Nil => value
        case head :: Nil if head == key => value
        case head :: tail =>
          children.indexWhere(child => child.key == head) match {
            case -1 => throw new RuntimeException("Bad Input")
            case index => children(index).findAValueInternal(tail)
          }
      }
    }

    def findAValue(path: String): String = {
      findAValueInternal(path.split('/').toList.tail)
    }

  }

  val test = new FolderNode("/", "")

  test.createPath("/a", "dataA")
  test.createPath("/a/b", "dataB")
  test.createPath("/c", "dataB")
  test.createPath("/c/d", "dataB")
  test.createPath("/e", "dataE")
  test.createPath("/a/b/F", "dataF1")
  test.createPath("/c/d/F", "dataF2")
  test.printFolders()
  println("=====================")
  println(test.findAValue("/a"))
  println(test.findAValue("/c/d/F"))
  println(test.findAValue("/a/b/F"))
//  println(test.findAValue("/e/F"))










}
