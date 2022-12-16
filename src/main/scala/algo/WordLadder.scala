package algo

/*
Given two words, beginWord and endWord, and a dictionary wordList,
return the number of words in the shortest transformation sequence
from beginWord to endWord, or 0 if no such sequence exists.
 */
object WordLadder extends App {

//  def oneStepAway(w1: String, w2: String) : Boolean = {
//    w1.nonEmpty & w2.nonEmpty && w1.length == w2.length && w1.diff(w2).length == 1
//  }
//
//  def oneStepAwayLonger(w1: String, w2: String): Boolean = {
//    w1.nonEmpty & w2.nonEmpty && w1.length == w2.length && {
//      val mpa1 = w1.foldLeft(Map[Char,Int]()) { (mapInProgress, c) =>
//        mapInProgress + (c -> (mapInProgress.getOrElse(c, 0) + 1))
//      }
//      val map2 = w2.foldLeft(mpa1) { (mapInProgress2, c) =>
//        val newCount = if (mapInProgress2.contains(c)) mapInProgress2(c) - 1 else 1
//        mapInProgress2 + (c -> newCount)
//      }
//
//      map2.values.filter(_ != 0).toList.length == 2
//    }
//  }
//
//
//  def ladderLength(beginWord: String, endWord: String, wordList: List[String]): Int = {
//    def ladderLengthInternal(currentWord: String, wordListTrimmed: List[String]): Option[Int] = {
//        val possibleMoves = wordListTrimmed.filter(oneStepAway(endWord, _)).toList
//        if (possibleMoves.isEmpty) None else {
//          if (possibleMoves.contains(endWord)) Some(1)
//          else {
//            possibleMoves.map( w => ladderLengthInternal(w, wordListTrimmed.filter(_ != w)))
//          }
//        }
//    }
//    if (wordList.contains(beginWord)) {
//      ladderLengthInternal(beginWord, wordList.filter(_ != beginWord))
//    } else -1
//

//  }

  val startWord = "hit"
  val endWord = "cog"
  val wordList = List("hot","dot","dog","lot","log","cog")
}
