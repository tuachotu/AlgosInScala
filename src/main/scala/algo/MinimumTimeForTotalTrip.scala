package algo

/*
[(1,2,3) 5]

1 + ([(1,2,3) 4])  2 ([(1,2,3) 4])  3 ([(1,2,3) 4])

[(1,2,3) 4]
1
 */
object MinimumTimeForTotalTrip extends App {
  def minimumTime(time: Array[Int], totalTrips: Int): Long = {
    var left: Long = 1L
    var right: Long = time.min * totalTrips.toLong
    while (left < right) {
      val moment = (right + left) / 2
      totalTripsAtMoment(time, moment = moment) match {
        case trips if trips >= totalTrips =>
          right = moment
        case _ =>
          left = moment + 1
      }
    }
    left
  }

  private def totalTripsAtMoment(time: Array[Int], moment: Long): Long = {
    time.foldLeft(0L) {
      case (s, v) => s + moment / v
    }
  }
  println(minimumTime(Array(1,2,3), 5))
}
