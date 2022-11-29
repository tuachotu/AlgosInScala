package string

import collection.mutable.PriorityQueue
import collection.mutable.HashMap

//scala> collection.mutable.PriorityQueue(7, 5, 2, 3, 1)(Ordering[Int].reverse).dequeueAll
//val res45: Seq[Int] = ArraySeq(1, 2, 3, 5, 7)
//
//scala> collection.mutable.PriorityQueue(7, 5, 2, 3, 1)(Ordering[Int]).dequeueAll
//val res46: Seq[Int] = ArraySeq(7, 5, 3, 2, 1)

object WordFrequency extends App {
  case class wordFreq(w: String, c: Int) extends Ordered[wordFreq] {
    //override def compare(that: wordFreq): Int = c compareTo (that.c)
    override def compare(that: wordFreq): Int = that.c compareTo (c)
  }

  val words = "aa bbb  ccc aa ddd aa ccc" split " +"
  val wordCount = HashMap[String, Int]()
  for (word <- words) {
    wordCount += (word -> (wordCount.getOrElse(word, 0) + 1))
  }
  val myHeap = PriorityQueue[wordFreq]()
  wordCount.toSeq foreach { case (w, c) => myHeap.enqueue(wordFreq(w, c)) }
  //println(myHeap.dequeueAll.map(_.w).mkString(","))
  println(myHeap.dequeueAll.toList)
}
