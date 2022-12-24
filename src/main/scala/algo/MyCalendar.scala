package algo

import scala.collection.mutable
object MyCalendar extends App {
  case class Event(start: Int, end: Int) extends Ordered[Event] {
    override def compare(that: Event): Int = this.start.compare(that.start)
  }


  class calendar {
    val events =  mutable.ArrayBuffer[Event]()

    private def findPos(e: Event): Option[Int] = {
      def findPosInternal(start: Int, end: Int): Option[Int] = {
        println(start, end)
        if (start > end || start >= events.length || end < 0) None
        else {
          val mid = (start + end) / 2
          //println(mid)
          mid match {
            case _ if events(mid).start <= e.start && events(mid).end >= e.end => Some(-1) // conflict
            case _ if events(mid).start > e.start => findPosInternal(start, mid - 1)
            case _ if events(mid).start < e.start => findPosInternal(mid + 1, end)
            case _ => Some(mid)
          }
        }
      }
      if (events.isEmpty) Some(0)
      else {
        findPosInternal(0, events.length )
      }
    }

    def book(event: Event): Boolean = {
      findPos(event) match {
        case Some(pos) =>
          println(pos)
          if (pos == 0) {
            events += event
            true
          } else if (pos == -1)  { false }
          else if (pos <= events.length) {
            if (events(pos).end <= event.start || events(pos).start > event.end ) {
              events += event
              events.sortInPlace()
              true
            }  else false
          } else false
        case None =>
          println("here")
          if (events.last.end <= event.start || events.last.start > event.end) {
            events += event
            events.sortInPlace()
            true
          } else false
      }

    }
   }

  val c = new calendar
  println(c.book(Event(10,20)))
  println(c.events)
  println(c.book(Event(22,25)))
  println(c.events)
  println(c.book(Event(12, 15)))
  println(c.events)

}
