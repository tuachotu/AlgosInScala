package Searching

import scala.annotation.tailrec

/*
https://leetcode.com/problems/capacity-to-ship-packages-within-d-days/description/
A conveyor belt has packages that must be shipped from one port to another within days days.

The ith package on the conveyor belt has a weight of weights[i].
Each day, we load the ship with packages on the conveyor belt (in the order given by weights).
We may not load more weight than the maximum weight capacity of the ship.

Return the least weight capacity of the ship that will result in all the packages on the conveyor belt being shipped within days days.
Input: weights = [1,2,3,4,5,6,7,8,9,10], days = 5
Output: 15
Input: weights = [3,2,2,4,1,4], days = 3
Output: 6
Input: weights = [1,2,3,1,1], days = 4
Output: 3
 */
object CapacityToShipPackagesWithinDDays extends App {
  def shipWithinDays(weights: Array[Int], days: Int): Int = {
    def canShipWithCapacity(c: Int): Boolean = {
      val (totalDaysNeeded, _ ) = weights.foldLeft((1, 0)) { case ((daysNeeded,weightShipped), w) =>
        if (weightShipped + w <= c ) (daysNeeded, weightShipped + w) else (daysNeeded + 1, w)
      }
      totalDaysNeeded <= days
    }
    @tailrec
    def shipWithinDaysInternal(left: Int, right: Int): Int = {
      (left,right) match {
        case _ if left > right => left
        case _ =>
          (left + right) / 2 match {
            case mid if canShipWithCapacity(mid) => shipWithinDaysInternal(left, mid-1)
            case mid => shipWithinDaysInternal( mid+1, right)
          }
      }
    }

    val l = weights.max
    val r = weights.sum
    shipWithinDaysInternal(l,r)
  }

  println(shipWithinDays(Array(1,2,3,4,5,6,7,8,9,10), 5))
  println(shipWithinDays(Array(3,2,2,4,1,4), 3))
  println(shipWithinDays(Array(1,2,3,1,1), 4))
}
