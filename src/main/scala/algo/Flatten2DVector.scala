package algo

import scala.collection.mutable.ListBuffer

object Flatten2DVectorSolution extends App {

  abstract class MyIterator {
    def hasNext: Boolean
    def next: Int
  }

  object Vector2D {
    def apply(_vec: Array[Array[Int]]): Vector2D = {
      new Vector2D(_vec.flatten.toList)
    }
  }
  class Vector2D(var myArray: List[Int] = List.empty[Int]) extends MyIterator {
    def hasNext: Boolean = myArray.nonEmpty
    def next: Int = {
      val t = myArray.head
      myArray = myArray.tail
      t
    }
  }



  class Better2DIterator(myArray: List[List[Int]]) {
    var row = -1
    var col = -1
    def calculateNextPos: Option[(Int,Int)] = {
      (row,col) match {
        case _ if row < myArray.length && col < myArray(row).length =>
          if (col+1 < myArray(row).length) Some((row,col+1))
          else if (row + 1 < myArray.length) Some((row+1,0))
          else None
        case _ => None
      }
    }
    def hasNext: Boolean = calculateNextPos.isDefined
    def next: Int = {

      val temp = if (row == -1 && col == -1) (0,0) else calculateNextPos.get
      println(temp,row,col)
      row = temp._1
      col = temp._2

      myArray(row)(col)
    }

  }

  val input1 = Vector2D(Array(Array(1,2), Array(3), Array(4)))
  println(input1.myArray.mkString(","))
  println(input1.next)
  println(input1.next)
  println(input1.next)
  println(input1.hasNext)
  println(input1.hasNext)
  println(input1.next)
  println(input1.hasNext)


  println()

  val input2 = new Better2DIterator(List(List(1, 2), List(3), List(4)))
  println(input2.next)
  println(input2.next)
  println(input2.next)
  println(input2.hasNext)
  println(input2.hasNext)
  println(input2.next)
  println(input2.hasNext)


}
