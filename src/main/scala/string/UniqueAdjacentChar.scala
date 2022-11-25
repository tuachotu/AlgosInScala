//package string
//
//import scala.collection.mutable.ArrayBuffer
//
////Rearrange characters in a String such that no two adjacent characters
//// are same using Frequency:
//object UniqueAdjacentChar extends App {
//// Step 1 - Build a char to count map
//// Step 2 - MAx element > (N + 1) /2 - not possible
//// Step 3 - fill the even numbers in result
//// Step 4 - fille the odd numbers
//
//
//  def buildCharCountMap(s: String): Map[Char, Int] = {
//    s.foldLeft(Map[Char,Int]()) { (charCountMap, c) =>
//      charCountMap + (c -> (charCountMap.getOrElse(c, 0) + 1) )
//    }
//  }
//
//  def rearrangeString(s: String): Option[String] ={
//    if (s.isEmpty) None else {
//      val charMap = buildCharCountMap(s)
//      println(charMap)
//      val r = ArrayBuffer.fill(s.length)(' ')
//      val (maxChar, maxCharCount) = charMap.reduceLeft((w1,w2) => if (w1._2 > w2._2) w1 else w2)
//      val restOfCharMap = charMap - maxChar
//
//      val (m1,m2, ccmap)= (0 until  s.length by 2).foldLeft((maxChar, maxCharCount, restOfCharMap)) { case ((mc, mcc, rcm), evenIndex) =>
//          r(evenIndex) = mc
//          if (mcc - 1 == 0 ) {
//            val (c1,cnt) = rcm.reduceLeft((w1,w2) => if (w1._2 > w2._2) w1 else w2)
//            val m = rcm - c1
//            (c1,cnt,m)
//          } else {
//            (mc, mcc-1, rcm)
//          }
//      }
//
//      (1 until s.length by 2).foldLeft(m1,m2,ccmap){ case ((mc, mcc, rcm), oddIndex) =>
//        r(oddIndex) = mc
//
//
//      }
//
//
//
//
//        None
//      }
//    }
//
//
//  println(rearrangeString("bbbbaaa123466"))
//
//
//}
