package string

object EditDistance extends  App {

  def MinOfThree(x: Int, y: Int, z: Int): Int = Math.min(x, Math.min(y,z))

  /*
   Solution
   Boundary Conditions - if both strings empty => 0
                         if one string empty, length of other string is result
   If first chars are same, call the same fucntions for tails
   Else three option
   work on first char of one string, i.e. pending will be editDistanceForStrings(s1.tail, s2)
   work on first char of second string, i.e. pending will be editDistanceForStrings(s1, s2.tail)
   work on rest of chars of both string i.e. editDistanceForStrings(s1.tail, s2.tail)

   repeat this
   */
  def editDistanceForStrings(s1: String, s2: String): Int = {
    (s1, s2) match {
      case _ if s1.isEmpty && s2.isEmpty => 0
      case _ if s1.isEmpty => s2.length
      case _ if s2.isEmpty => s1.length
      case _ if s1.head == s2.head => editDistanceForStrings(s1.tail, s2.tail)
      case _ => 1 + MinOfThree(editDistanceForStrings(s1, s2.tail), editDistanceForStrings(s1.tail, s2), editDistanceForStrings(s1.tail, s2.tail))
    }
  }

  println(editDistanceForStrings("cat", "cut"))
  println(editDistanceForStrings("sunday", "saturday"))
}
