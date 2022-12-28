package algo

/*
Example 1:
Input: words = ["hello","leetcode"], order = "hlabcdefgijkmnopqrstuvwxyz"
Output: true
Explanation: As 'h' comes before 'l' in this language, then the sequence is sorted.

Example 2:
Input: words = ["word","world","row"], order = "worldabcefghijkmnpqstuvxyz"
Output: false
Explanation: As 'd' comes after 'l' in this language, then words[0] > words[1], hence the sequence is unsorted.

Example 3:

Input: words = ["apple","app"], order = "abcdefghijklmnopqrstuvwxyz"
Output: false
Explanation: The first three characters "app" match, and the second string is shorter (in size.) According to lexicographical rules "apple" > "app", because 'l' > '∅', where '∅' is defined as the blank character which is less than any other character (More info).

 */
object VerifyAlientDictionary extends  App {
  def createDict(s: String): List[Int] = {
    val dict = Array.fill(26)(0)
    s.indices.foldLeft(dict) { (dictInProgress, cIndex) =>
      dictInProgress(s(cIndex) - 'a') = cIndex
      dictInProgress
    }.toList
  }

  def isAlienSorted(words: Array[String], order: String): Boolean = {
    val dictionary = createDict(order)

    def twoWordsOfSameLengthInOrder(w1: String, w2: String): Boolean = {
      (for {
        a <- w1
        b <- w2
      } yield (a, b)).toList.indexWhere { pair =>
        dictionary(pair._1 - 'a') != dictionary(pair._2 - 'a')
      } match {
        case -1 => true
        case p => dictionary(w1(p) - 'a') <= dictionary(w2(p) - 'a')
      }
    }

    def compareWords(w1: String, w2: String): Boolean = {
      if (w1.length != w2.length) {
        val lengthToCompare = Math.min(w1.length, w2.length)
        if (twoWordsOfSameLengthInOrder(w1.take(lengthToCompare), w2.take(lengthToCompare))) {
          w1.length <= w2.length
        } else false
      } else twoWordsOfSameLengthInOrder(w1, w2)
    }

    words.toList.sliding(2).toList.forall { case List(w1,w2) => compareWords(w1, w2)}

  }
  println(isAlienSorted(Array("hello","leetcode"), "hlabcdefgijkmnopqrstuvwxyz"))
  println(isAlienSorted(Array("word","world","row"),"worldabcefghijkmnpqstuvxyz"))
  println(isAlienSorted(Array("apple","app"),"abcdefghijklmnopqrstuvwxyz"))
}
