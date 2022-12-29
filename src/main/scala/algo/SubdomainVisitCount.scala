package algo

object SubdomainVisitCount extends App {
  def stringToDomainList(s: String): List[String] = s.split ('.').toList match {
    case d3 :: d2 :: d1 :: Nil => List(s"$d3.$d2.$d1", s"$d2.$d1", s"$d1")
    case d2 :: d1 :: Nil => List(s"$d2.$d1", s"$d1")
    case _ => List.empty[String]
  }

  def subdomainVisits(cpdomains: Array[String]): List[String] = {
    cpdomains.foldLeft(Map.empty[String, Int]) { case (visitMap, cpd) =>
      cpd.split(" +").toList match {
        case count :: domainString :: Nil =>
          stringToDomainList(domainString).foldLeft(visitMap) { (resMap, d) =>
            resMap + (d -> resMap.getOrElse(d,count.toInt))
          }
        case _ => visitMap
      }
    }.toList.map { case (k,v) => s"$v $k"}

  }

  //println(subdomainVisits(Array("9001 discuss.leetcode.com")))
  println(subdomainVisits((Array("900 google.mail.com", "50 yahoo.com", "1 intel.mail.com", "5 wiki.org"))))

}
