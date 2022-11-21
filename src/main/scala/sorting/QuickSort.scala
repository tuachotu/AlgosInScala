package sorting

object QuickSort extends App {
  def quickSort(l: List[Int]): List[Int] = {
   val mid = l.length / 2
   if (mid == 0) l
   else {
     val pivot = l(mid)
     val lessThanPivot = l.filter(_ < pivot)
     val moreThanPivot = l.filter(_ > pivot)
     val allPivot = l.filter(_ == pivot)
     quickSort(lessThanPivot)++allPivot++quickSort(moreThanPivot)
   }
  }
  //println(quickSort(List(9,8,7,6,5,4,3,2,1)).mkString(","))
  println(quickSort(List(9,18,27,36,56,4,34,22,1)).mkString(","))
  //println(quickSort(List(9,8,7,6,5,4,3,2,1)).mkString(","))


}
