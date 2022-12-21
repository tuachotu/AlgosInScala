package algo

object MyHashMap extends App {
  abstract class MySimpleHashMap{
    def length: Int
    def get(key: Int): Int
    def put(key: Int, value: Int): Unit
    def isEmpty: Boolean
  }

  class MySimpleHashMapImplementation(var block: Array[Int] = Array.fill[Int](10000)(-1), var size: Int = 0) extends MySimpleHashMap {
    def get(key: Int): Int = block(getIndex(key))
    def put(key: Int, value: Int): Unit = {
      size = size + 1
      block(getIndex(key)) = value
    }
    def isEmpty: Boolean = size == 0
    def length: Int = size
    private def getIndex(key:Int):Int = key.hashCode()%10000
  }

//  class MySimpleHashMapImplementation(var block: Array[(Int, Int)] = Array.fill[(Int, Int)](10000)(-1, -1), var size: Int = 0) extends MySimpleHashMap {
//    def get(key: Int): Int = block(getIndex(key))
//
//    def put(key: Int, value: Int): Unit = {
//      size = size + 1
//      block(getIndex(key)) = value
//    }
//
//    def isEmpty: Boolean = size == 0
//
//    def length: Int = size
//
//    private def getIndex(key: Int): Int = key.hashCode() % 10000
//  }

}
