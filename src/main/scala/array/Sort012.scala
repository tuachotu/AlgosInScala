package array

object Sort012 extends App {
  def sortArrayV1(nums: List[Int]): List[Int] = {
    val (oneCount, twoCount, threeCount) = nums.foldLeft((0,0,0)) { case ((oneCountTemp, twoCountTemp, threeCountTemp), number) =>
      number match {
        case 1 => (oneCountTemp + 1, twoCountTemp, threeCountTemp)
        case 2 => (oneCountTemp, twoCountTemp + 1, threeCountTemp)
        case 3 => (oneCountTemp, twoCountTemp, threeCountTemp + 1)
      }
    }
    List.fill(oneCount)(1) ++ List.fill(twoCount)(2) ++ List.fill(threeCount)(3)
  }

  def sortArrayV2(nums: List[Int]): List[Int] = {
    nums.filter(_ == 1) ++ nums.filter(_ == 2) ++ nums.filter(_ == 3)
  }


  println(sortArrayV1(List(1,1,1,1,2,2,1,2,1,2,1,2,3,1,3,3,3,2)))
  println(sortArrayV2(List(1,1,1,1,2,2,1,2,1,2,1,2,3,1,3,3,3,2)))

}
