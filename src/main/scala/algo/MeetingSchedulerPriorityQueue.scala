package algo

import scala.collection.mutable.PriorityQueue
object MeetingSchedulerPriorityQueue extends App {
  case class Availability(start: Int, end: Int) extends Ordered[Availability] {
    override def compare(that: Availability): Int = that.start.compare(this.start)
  }

  def minAvailableDuration(slots1: Array[Array[Int]], slots2: Array[Array[Int]], duration: Int): List[Int] = {
    val person1Avail = slots1.map(t => Availability(t(0), t(1))).toList
    val person2Avail = slots2.map(t => Availability(t(0), t(1))).toList

    val myMinHeap = PriorityQueue.empty[Availability]
    (person2Avail ++ person1Avail) foreach { a => if ((a.end - a.start) >= duration) myMinHeap.enqueue(a) }
    var b = false
    var res: Option[List[Int]] = None
    while(myMinHeap.size > 1 && !b) {

      val head = myMinHeap.dequeue()
      val next = myMinHeap.dequeue()
      if ((next.start + duration) <= head.end) {
        res= Some(List(next.start, next.start + duration))
        println(res)
        b =true
      }
      myMinHeap.enqueue(next)
    }
    res match {
      case Some(x) => x
      case None => List.empty[Int]
    }

  }


  print(minAvailableDuration(Array(Array(10,50), Array(60,120), Array(140,210)), Array(Array(0,15), Array(60,70)), 8))



}
