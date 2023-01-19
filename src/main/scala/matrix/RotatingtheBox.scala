package matrix

object RotatingtheBox extends App {
/*
take col find index of block,
take array for block, move all floaters to the block-1
 */
  def rotatingtheBoxBy90(box: List[List[Char]]): List[List[Char]] = {
    (box.head.indices.toList map { col =>
      box.indices.toList map { row =>
        box(row)(col)
      }
    }).map(_.reverse)
  }
  def moveElementsInRow(ll: List[Char]): List[Char] = {
    val obstacle = '*'
    val stone = '#'
    val empty = '.'
    var emptySpot = ll.length - 1
    val newList = Array.fill[Char](ll.length)(empty)
    ll.indices.reverse foreach { index =>
      if (ll(index) != empty) {
        if(ll(index) == obstacle) {
          newList(index) = obstacle
          emptySpot = index -1
        } else { // stone
          newList(emptySpot) = stone
          emptySpot = emptySpot - 1
        }
      }
    }
    newList.toList
  }

  def movingElementsToFutureBottom(box: List[List[Char]]): List[List[Char]] = {
    box.map(moveElementsInRow)
  }

  def rotateBox(box: List[List[Char]]): List[List[Char]] = {
    rotatingtheBoxBy90( movingElementsToFutureBottom(box))
  }

  val input1 = List(List('*','#','.','#', '*', '.','#','*'))
  input1 foreach( row => println(row.mkString(",")))
  println()
  rotateBox(input1) foreach( row => println(row.mkString(",")))
  println()
  val input2 = List(List('#', '.', '*', '.'), List('#', '#', '*', '.'))
  input2 foreach (row => println(row.mkString(",")))
  println()
  rotateBox(input2) foreach (row => println(row.mkString(",")))

}
