package algo
//Given n pairs of parentheses, write a function to generate all combinations of well-formed parentheses.
// ((()))

// Solution -
// recursive solution
// we need to start by
// result = "", pairs = List((),(),()), pendingClosing = Empty List
// Step 1
// - put one open paren in result string, and matching close parent in the list
// result = "(", pairs = List((),()), pendingClosing = List[ ) ]
// Step 2
// - Check if pairs is empty && pendingClosing is empty, return result
// - if not, check if only pairs is empty, append all pending to result and return
// - else {
//   two options , add a new ( and make recrusive call, add a new ) and make a recursive call
//  }
// result = "(", pairs = List((),()), pendingClosing = List[ ) ]


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
      getAllParensInternal(parens.head._1, parens.tail, List(parens.head._2))
    }

    }


  println(getAllParens(0).mkString(","))
  println(getAllParens(2).mkString(","))
  println(getAllParens(1).mkString(","))
  println(getAllParens(3).mkString(","))
  }


