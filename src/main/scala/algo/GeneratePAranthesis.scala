package algo
//Given n pairs of parentheses, write a function to generate all combinations of well-formed parentheses.
// ((()))


object GeneratePAranthesis extends App{

  def getAllParens(n:Int): List[String] = {
    def getAllParensInternal(s: String, parenPairs: List[(String, String)], closeParens: List[String]): List[String] = {
      if (parenPairs.isEmpty && closeParens.isEmpty)  List(s)
      else if (parenPairs.isEmpty) List(s+closeParens.mkString(""))
      else {
        getAllParensInternal(s + parenPairs.head._1, parenPairs.tail, List(parenPairs.head._2) ++ closeParens) ++
          (if (closeParens.isEmpty) Nil else  getAllParensInternal(s + closeParens.head, parenPairs, closeParens.tail))
      }
    }
    if (n == 0) Nil else {
      val parens = List.fill(n)(("(", ")"))
      val closeParens = List[String]()
      getAllParensInternal(parens.head._1, parens.tail, List(parens.head._2))
    }

    }


  println(getAllParens(0).mkString(","))
  println(getAllParens(2).mkString(","))
  println(getAllParens(1).mkString(","))
  println(getAllParens(3).mkString(","))
  }


