package algo

object FindCommonSpaces extends App {
 def findSpace(strings: List[String]): List[Int] = {
   strings.foldLeft((List.empty[Int], 0)) { case ((indexForLastString, lastStringSize), currentString) =>
     val indexForCurrentString = currentString.indices.foldLeft(indexForLastString) { (currentStringIndexes, index) =>
       currentString(index) match {
         case c if c !=' ' => currentStringIndexes.filter(_ != index)
         case _ if index > lastStringSize => index::currentStringIndexes
         case _ => currentStringIndexes
       }
    }
     (indexForCurrentString, Math.max(currentString.length,lastStringSize ))
   }._1
 }

 val input1 = List("aa bb ccc d", "axxbb ccc d")
  println(findSpace(input1))
}
