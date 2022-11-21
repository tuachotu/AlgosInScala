package string

object SubStrings extends App {
  def allSubStrings(s:String): List[String] = {
    ((0 until s.length) flatMap { pos =>
      val stringToWorkOn = s.drop(pos)
      (1 to stringToWorkOn.length) map { charToTake =>
        stringToWorkOn.take(charToTake)
      }
      }).toList.filter(!_.isEmpty)
  }


  println(allSubStrings("abc").mkString(","))


}
