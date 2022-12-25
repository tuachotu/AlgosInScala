package algo

object PourWater extends App{
  def pourWater(heights: Array[Int], volume: Int, k: Int): Array[Int] = {
      def pourWaterInternal(start: Int, moveLeft: Boolean,  volumeToDistr: Int):  Int = {

          var volumeToDistribute = volumeToDistr
          var startIndex = start
          var keepMoving = true
          var volumeUsed = 0

          while(keepMoving && volumeToDistribute > 0 ) {
            startIndex = if (moveLeft) startIndex - 1 else startIndex + 1
            println(startIndex, moveLeft, volumeToDistr, volumeUsed, heights.mkString(","))
            if (startIndex < 0 || startIndex >= heights.length) keepMoving = false else {
              heights(startIndex) match {
                case v if v >= heights(k) => keepMoving = false
                case v =>
                  val volumeNeededAtIndex = heights(k) - v
                  if (volumeNeededAtIndex >= volumeToDistribute) {
                    heights(startIndex) = v + volumeToDistribute
                    volumeUsed =  volumeUsed + volumeToDistribute
                    keepMoving = false
                  } else { //volumeNeededAtIndex < volumeToDistribute
                    heights(startIndex) = heights(k)
                    volumeToDistribute = volumeToDistribute - volumeNeededAtIndex
                    volumeUsed = volumeUsed + volumeNeededAtIndex
                  }
              }
            }
          }

        volumeUsed
      }

      var keepPouring = true
      var pendingVolume = volume
      while (keepPouring) {
        //println(heights.mkString(","))
        // check left
        val waterUsed1 = pourWaterInternal(k, true, pendingVolume)
        //println(waterUsed1,1)
        if (waterUsed1 == pendingVolume) {
          keepPouring = false
        } else {
          val waterUsed2 = pourWaterInternal(k, false, pendingVolume - waterUsed1)
          //println(waterUsed2,2)
          if (waterUsed2 == pendingVolume - waterUsed1) {
            keepPouring = false
          } else {
            heights(k) = heights(k) + 1
            pendingVolume = pendingVolume - (waterUsed1 + waterUsed2 + 1)
            if (pendingVolume == 0)  keepPouring = false
          }
        }
      }
    heights
  }

  val volume3 = 2
  val input3 = Array(1,2,3,4)
  val k3 = 2

  println(pourWater(input3, volume3, k3).mkString(",")) //  [2,2,2,3,2,2,2]

//  val volume1 = 4
//  val input1 = Array(2,1,1,2,1,2,2)
//  val k1 = 3
//
//  println(pourWater(input1, volume1,k1).mkString(",")) //  [2,2,2,3,2,2,2]
//
//
//  val input2 = Array(3,1,3)
//  val volume2 = 5
//  val k2 = 1
//
//  println(pourWater(input2, volume2, k2).mkString(",")) //  [2,2,2,3,2,2,2]
}
