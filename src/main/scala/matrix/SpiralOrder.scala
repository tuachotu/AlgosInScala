package matrix

//Given an m x n matrix, return all elements of the matrix in spiral order.
//Input: matrix = [[1,2,3],[4,5,6],[7,8,9]]
//Output: [1,2,3,6,9,8,7,4,5]

//Input: matrix = [[1,2,3,4],[5,6,7,8],[9,10,11,12]]
//Output: [1,2,3,4,8,12,11,10,9,5,6,7]


// Choose inner matrix
// eqch middle row drop head tail


object SpiralOrder extends App {

  def getInnerMatrix(m: List[List[Int]]): List[List[Int]] = {
    if (m.isEmpty || m.length == 1 || m.head.length == 1) List.empty[List[Int]] else {
        if (m.tail.init.nonEmpty) {
          m.tail.init.map { row =>
            if (row.tail.nonEmpty) row.tail.init else List.empty[Int]
          }
        } else List.empty[List[Int]]
    }
  }



  def GetSpiral(matrix: List[List[Int]]): List[Int] = {
    def getSprialInternal(m: List[List[Int]]): List[Int] = {
        if (m.isEmpty) List.empty[Int]
        else if (m.length == 1) m.head
        else {
          val innerMatrix = getInnerMatrix(m)
          val outer1 = m.head
          val outer3 = m.last.reverse
          val outer2 = if (m.tail.init.nonEmpty && m.head.length > 1) { // tricky part
              m.tail.init map  { row => row.tail.last }
            }  else List.empty[Int]
          val outer4 = if (m.tail.init.nonEmpty) m.tail.init map  { row => row.head } else List.empty[Int]
          println(outer1,outer2, outer3, outer4)
          outer1 ++ outer2 ++ outer3 ++ outer4 ++ getSprialInternal(innerMatrix)
        }
    }
    getSprialInternal(matrix)
  }




  val input1 = List(List(1,2,3),List(4,5,6),List(7,8,9))
  val input2 = List(List(1,2,3,4),List(5,6,7,8),List(9,10,11,12))
  val input3 = List(List(1),List(2),List(3))
  val input4 = List(List(1,2,3,4))
  val input5 = List(List(1,11),List(2,22),List(3,33))

//  input3 foreach { row =>
//    row foreach { col => print(col , " ")}
//    println
//  }
//println()
//
//  println(GetSpiral(input3))


//  input2 foreach { row =>
//    row foreach { col => print(col, " ") }
//    println
//  }
//  println()
//
//  println(GetSpiral(input2))

  input3 foreach { row =>
    row foreach { col => print(col, " ") }
    println
  }
  println()

  println(GetSpiral(input3))

}
