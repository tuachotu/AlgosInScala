package string


//Example 1:
//
//Input: croakOfFrogs = "croakcroak"
//Output: 1
//Explanation: One frog yelling "croak" twice.
//Example 2:
//
//Input: croakOfFrogs = "crcoakroak"
//Output: 2
//Explanation: The minimum number of frogs is two.
//The first frog could yell "crcoakroak".
//The second frog could yell later "crcoakroak".
//Example 3:
//
//Input: croakOfFrogs = "croakcrook"
//Output: -1
//Explanation: The given string is an invalid combination of "croak" from different frogs.

import scala.collection.mutable

object FrogsCroak extends App {
  // use it to check if char have a mapping
  val prefixMap = Map("c" -> "", "r" -> "c","o" -> "cr","a" -> "cro","k" -> "croa")

  def countFrogs(s:String): Int = {
    val prefixTracker = mutable.HashMap[String, Int]()
    var count = 0

    def updateFrogCount(): Unit = {
      if(prefixTracker.contains("croak")) {
        val inProgressCroak = prefixTracker.count { case (k,v) =>
          k != "croak" && v != 0
        }
        if (prefixTracker("croak") < inProgressCroak) {
          count = count + 1
        }
      } else {
         count = count + 1
      }
    }


    val parseResult = (0 until s.length).toList takeWhile { index =>
      val c = s(index).toString
      prefixMap.get(c) match {
        case None => false // not a valid prefix
        case Some("") => // a new croak started
          updateFrogCount() // update count
          prefixTracker +=  (c -> (prefixTracker.getOrElse(c,0) + 1))
          true
        case Some(prefix) if !prefixTracker.contains(prefix) => false
        case Some(prefix) if !(prefixTracker(prefix) > 0)=> false
        case Some(prefix) => // we found  r, o, a, k
          prefixTracker += (prefix -> (prefixTracker(prefix) - 1))
          prefixTracker += (prefix + c -> (prefixTracker.getOrElse(prefix + c, 0) + 1))
          true
      }
    }
    if (parseResult.length == s.length) {
      val unfinishedCroak = prefixTracker.count { case (k,v) => (k != "croak") && (v > 0) }
      if (unfinishedCroak > 0) -1 else count



    } else -1
  }
  println(countFrogs("cccrorakrcoakorakoak"))
//  println(countFrogs("croakcroa"))
//  println(countFrogs("croak"))
//  println(countFrogs("crcoakroak"))
//  println(countFrogs("croakcrook"))

}
