package algo

object CountBinarySubStrings extends App {
  def isConsecutive(s: String): Boolean = {
  if (s.length > 1) {
    s.indices.tail.find(index => s(index - 1) != s(index)) match {
      case Some(index) =>
        s.take(index).length == s.drop(index).length
      case None => false
    }
  } else false
  }

  def countBinarySubstrings(s: String): Int = {
    (for {
      start <- s.indices
      end <- start to s.length
    } yield s.slice(start, end)).toList count { s =>
      isConsecutive(s)
    }
  }

  def gatherCount(s:String): List[Int]  = {
    s.tail.foldLeft((List(1), s.head)) { case ((resInProgress, lastChar), currentChar) =>
      if (currentChar == lastChar) {
        (resInProgress.init ++ List(resInProgress.last+ 1), lastChar)
      } else {
        (resInProgress++List(1), currentChar)
      }
    }._1
  }

  def countBinarySubstringsV2(s: String): Int = {
    gatherCount(s).sliding(2).toList.map { case List(s1,s2) =>
      Math.min(s1,s2)
    }.sum
  }

  println(countBinarySubstrings("00110011"))
  println(countBinarySubstrings("10101"))

  println(countBinarySubstringsV2("00110011"))
  println(countBinarySubstringsV2("10101"))

  println(gatherCount("00110011"))
  println(gatherCount("10101"))
}
