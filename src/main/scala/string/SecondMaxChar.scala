package string

object SecondMaxChar extends  App {
  case class CharCount(c: Char, count: Int) extends Ordered[CharCount] {
    override def compare(that: CharCount): Int = this.count.compareTo(that.count)
    //override def compare(that: CharCount): Int = that.count.compareTo(this.count)
  }

  def calaculateNthMaxChar(s: String, n: Int): Char  = {
    val charMap = s.foldLeft(Map.empty[Char, Int]) { case (mapInProgress, c) =>
      mapInProgress + ( c ->  (mapInProgress.getOrElse(c,0) + 1))
    }

    val heap = scala.collection.mutable.PriorityQueue.empty[CharCount]

    charMap.toList  map {case (k,v) => CharCount( k,v)} foreach (heap.enqueue(_))

//    val l = for {
//      i <- 0 until n
//    } yield heap.dequeue
//
//    l.last.c

    0 until n-1 foreach( _ => heap.dequeue())
    heap.head.c
  }

  println(calaculateNthMaxChar("zzzzzxxxttyyyy", 2))

}
