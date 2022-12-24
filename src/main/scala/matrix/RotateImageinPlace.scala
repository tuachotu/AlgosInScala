package matrix

/*
You are given an n x n 2D matrix representing an image, rotate the image by 90 degrees (clockwise).

You have to rotate the image in-place, which means you have to modify the input 2D matrix directly.
DO NOT allocate another 2D matrix and do the rotation.

 */
object RotateImageInPlace extends App {

  def reverseArray(l : Array[Int]): Unit = {
    var i = 0
    var j = l.length -1

    while ( i < j ) {
      val temp = l(i)
      l(i) = l(j)
      l(j) = temp
      i = i +1
      j = j -1
    }
  }

  def transpose(matrix: Array[Array[Int]]): Unit = {
    for {
      i <- 0 until matrix.length
      j <- i until matrix.head.length
    } {
      val temp = matrix(i)(j)
      matrix(i)(j) = matrix(j)(i)
      matrix(j)(i) = temp
    }
  }

  def rotate(matrix:  Array[Array[Int]]): Unit = {
    transpose(matrix)
    matrix foreach ( r => reverseArray(r))
  }

  val input1 = Array(Array(1,2,3),Array(4,5,6),Array(7,8,9))
  input1 foreach ( r => println(r.mkString(",")))
  rotate(input1)
  input1 foreach ( r => println(r.mkString(",")))

}
