package algo

object MeetingScheduler extends App {

  /*
  Solution -
  Merge both arrays
   */

  case class Availability(start: Int, end: Int)


  // check overlap
  def mergeAvailability(l1: List[Availability], l2: List[Availability]): List[Availability] = {
    (l1, l2) match {
      case (Nil, Nil) | (_, Nil) | (Nil, _) => Nil
      // Check overlapping
      case _ if (l1.head.start < l2.head.start && l1.head.end < l2.head.end) =>
        val commonslot = Availability(l2.head.start,l1.head.end)
        val pendingSlot = Availability(l1.head.end+1, l2.head.end)
        val newL2 = pendingSlot::l2.tail
        commonslot :: mergeAvailability(l1.tail, newL2)
      case _ if (l2.head.start < l1.head.start && l2.head.end < l1.head.end) =>
        val commonslot = Availability(l1.head.start, l2.head.end)
        val pendingSlot = Availability(l2.head.end + 1, l1.head.end)
        val newL1 = pendingSlot :: l1.tail
        commonslot :: mergeAvailability(l2.tail, newL1)

       // Check containted
      case _ if (l2.head.start < l1.head.start && l2.head.end > l1.head.end) =>
        val commonslot = l1.head
        val pendingSlot = Availability(l1.head.end + 1, l2.head.end)
        val newL2 = pendingSlot :: l2.tail
        commonslot :: mergeAvailability(l1.tail, newL2)
      case _ if (l1.head.start < l2.head.start && l1.head.end > l2.head.end) =>
        val commonslot = l2.head
        val pendingSlot = Availability(l2.head.end + 1, l1.head.end)
        val newL1 = pendingSlot :: l1.tail
        commonslot :: mergeAvailability(l2.tail, newL1)
      case _ if l1.head.end < l2.head.start => mergeAvailability(l1.tail, l2)
      case _ if l2.head.end < l1.head.start => mergeAvailability(l2.tail, l1)
      case _ =>
        Availability(Math.max(l1.head.start, l2.head.start), Math.min(l1.head.end, l2.head.end)) :: mergeAvailability(l2.tail, l1.tail)
    }
  }

  def minAvailableDuration(slots1: Array[Array[Int]], slots2: Array[Array[Int]], duration: Int): List[Int] = {
    val person1Avail = slots1.map(t => Availability(t(0), t(1))).toList
    val person2Avail = slots2.map(t => Availability(t(0), t(1))).toList

    val mergedSlots = mergeAvailability(person1Avail, person2Avail)
    mergedSlots.indexWhere( slot => (slot.end  - slot.start) >= duration) match {
        case -1 => List.empty[Int]
        case p => List(mergedSlots(p).start, mergedSlots(p).start + duration)
      }
  }

//  println(minAvailableDuration(Array(Array(10,50), Array(60,120), Array(140,210)), Array(Array(0,15), Array(60,70)), 8))
//  println(minAvailableDuration(Array(Array(10,50), Array(60,120), Array(140,210)), Array(Array(0,15), Array(60,70)), 12))
  //println(minAvailableDuration(Array(Array(10,60)), Array(Array(12,17), Array(21,50)), 8))
  println(minAvailableDuration(Array(Array(10,50), Array(60,120), Array(140,210)),
    Array(Array(0,15), Array(40,50)), 8))

}
