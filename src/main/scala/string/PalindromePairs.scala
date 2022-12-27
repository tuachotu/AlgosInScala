package string

/*
Example 1:

Input: words = ["abcd","dcba","lls","s","sssll"]
Output: [[0,1],[1,0],[3,2],[2,4]]
Explanation: The palindromes are ["abcddcba","dcbaabcd","slls","llssssll"]
Example 2:

Input: words = ["bat","tab","cat"]
Output: [[0,1],[1,0]]
Explanation: The palindromes are ["battab","tabbat"]
Example 3:

Input: words = ["a",""]
Output: [[0,1],[1,0]]
Explanation: The palindromes are ["a","a"]
 */

object PalindromePairs extends App {
  // a -> (a, ""), ab -> (ba, a)  , abc -> cba, ba
  // lls -> (sll,s)
  // s -> (s.reverse, r.reverse.tail)

  def isPalindrom(s: String): Boolean = {
    s match {
      case "" => true
      case _ if s.length == 1 => true
      case _ if s.length == 2 => s.head == s.last
      case _  => s.head == s.last  && isPalindrom(s.tail.init)
    }
  }
   def findPalindromePairsForWord(s: String): List[String] = {
     s match {
       case _ if s.isEmpty => List.empty[String]
       case _ if s.length == 1 => List(s.reverse)
       case _ =>
         val first = s.reverse
         val secondSuperSet = s.reverse.init.indices map {c =>
           s.reverse.init.take(c+1)
         }
         (first::secondSuperSet.toList).filter{ t =>  isPalindrom(s+t) ||isPalindrom(t+s) }
     }
  }

  def palindromePairs(words: Array[String]): List[(Int,Int)] = {
    val palindromLookupMap = words.indices.foldLeft(Map.empty[String,Int]) { case (palindromLookupMap, wordIndex) =>
          findPalindromePairsForWord(words(wordIndex)).map(w => (w, wordIndex)).toMap ++ palindromLookupMap
        }
    words.indices.foldLeft(List.empty[(Int, Int)]) { case (res, wordIndex) =>
      if (palindromLookupMap.contains(words(wordIndex))) (wordIndex, palindromLookupMap(words(wordIndex)))::res else res
    }
  }
  //println(isPalindrom("llss"))
//  println(palindromePairs(Array("bat","tab","cat")))
 println(palindromePairs(Array("abcd","dcba","lls","s","sssll")))

}
