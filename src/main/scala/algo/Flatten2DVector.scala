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

  val input1 = Vector2D(Array(Array(1,2), Array(3), Array(4)))
  println(input1.myArray.mkString(","))
  println(input1.next)
  println(input1.next)
  println(input1.next)
  println(input1.hasNext)
  println(input1.hasNext)
  println(input1.next)
  println(input1.hasNext)


}
