package algo
import scala.collection.mutable.ArrayBuffer
object PhoneNumberToString extends App {

  val lookup: Map[Char, String] = Map(('2' -> "abc") , ('3' -> "def"), ('4' -> "ghi"), ('5' -> "jkl"),
    ('6'-> "mno"), ('7' -> "pqrs"), ('8'-> "tuv"), ('9' -> "wxyz"))
  var resultList = ArrayBuffer[String]()

  def phoneNumberToString(num: String): List[String] = {

    def buildResult(index: Int, result: String): Unit = {
      if (result.length == num.length) { resultList :+= result }
      else {
        val stringToChooseFrom = lookup(num(index))
        stringToChooseFrom foreach { c =>
          val resMod = result+c
          buildResult(index+1, resMod)

        }
      }
    }
    if (num.isEmpty) Nil
    else  {
      buildResult(0, "")
      resultList.toList
    }
  }

  println(phoneNumberToString("23"))

}
