package string

object SubstringOnLengthN extends App {
  def substrings(s: String, l: Int): List[String] = {
    if (s.isEmpty || s.length < l ) Nil
    else {
      ((0 until s.length - l  +1  ) map { index => s.drop(index).take(l)}).toList
      }
    }

  println(substrings("Vikrant", 5).mkString(","))

}
