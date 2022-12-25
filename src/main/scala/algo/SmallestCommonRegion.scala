package algo

object SmallestCommonRegion extends App {
  def parseRegion(regions: List[List[String]]): Map[String, List[String]] = {
    regions.foldLeft(Map.empty[String, List[String]]) { case(regionMapInProgress, regionList) =>
      val updatedMap = regionMapInProgress map { case (k,v) =>
        if (v.contains(regionList.head)) {
          val regionToAdd = regionList.tail.filter(a => !v.contains(a))
          k ->  (v ++ regionToAdd)
        } else {
          k -> v
        }
      }
      updatedMap + (regionList.head -> regionList.tail)
    }
  }
  def findSmallestRegion(regions: List[List[String]], region1: String, region2: String): String = {
    val parsedInfo = parseRegion(regions)

    parsedInfo.foldLeft((0,"")) { case ((resLength, key), (regionHead, regionAreas)) =>
      if (regionAreas.contains(region1) && regionAreas.contains(region2)) {
        println(regionAreas)
        if (resLength > regionAreas.length || resLength == 0  ) (regionAreas.length , regionHead) else (resLength, key)
      } else {
        (resLength, key)
      }
    }._2
  }

  val regions =
    List(List("Earth", "North America", "South America"),
      List("North America", "United States", "Canada"),
      List("United States", "New York", "Boston"),
      List("Canada", "Ontario", "Quebec"),
      List("South America", "Brazil")
    )
  val region1 = "Quebec"
  val region2 = "New York"

  println(findSmallestRegion(regions, region1,region2 ))
}
