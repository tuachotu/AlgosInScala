package string

object StringMatchingNaiveAlgo extends App {
  def patternMatch(m: String, n: String): Unit = {
    if (m.nonEmpty && n.nonEmpty && m.length > n.length) {
      0 until m.length foreach { index =>
        if (m.drop(index).startsWith(n)) println(s"pattern found at $index")
      }
    }
  }

  def patternFoundAt(m: String, n: String): List[Int] = {
    if (m.nonEmpty && n.nonEmpty && m.length > n.length) {
      (0 until m.length).foldLeft(List[Int]()){ (result,index) =>
        if (m.drop(index).startsWith(n)) result ++ List(index) else result
      }
    } else Nil
  }

  def patternFirstFoundAt(m: String, n: String): Int = {
    if (m.nonEmpty && n.nonEmpty && m.length >= n.length) {
       (0 until m.length).collectFirst { case index if m.drop(index).startsWith(n) => index } match {
         case Some(index) => index
         case _ => -1
       }
    } else -1
  }
//  patternMatch("AAABBCC", "2ABB")
//  println( patternFoundAt("AAABBCC", "2ABB").mkString(","))
//
//  println( patternFoundAt("AAABBCC", "AA").mkString(","))

  println( patternFirstFoundAt("AAABBCC", "AA"))
  println( patternFirstFoundAt("1AAABBCC", "AA"))
  println( patternFirstFoundAt("A", "A"))
}
