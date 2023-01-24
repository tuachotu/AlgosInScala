package algo

/*
Example 1:

Input: words = ["This", "is", "an", "example", "of", "text", "justification."], maxWidth = 16
Output:
[
   "This    is    an",
   "example  of text",
   "justification.  "
]
Example 2:

Input: words = ["What","must","be","acknowledgment","shall","be"], maxWidth = 16
Output:
[
  "What   must   be",
  "acknowledgment  ",
  "shall be        "
]
Explanation: Note that the last line is "shall be    " instead of "shall     be", because the last line must be left-justified instead of fully-justified.
Note that the second line is also left-justified because it contains only one word.
Example 3:

Input: words = ["Science","is","what","we","understand","well","enough","to","explain","to","a","computer.","Art","is","everything","else","we","do"], maxWidth = 20
Output:
[
  "Science  is  what we",
  "understand      well",
  "enough to explain to",
  "a  computer.  Art is",
  "everything  else  we",
  "do                  "
]
*/

object TextJustification extends App {
  def fillSpace(words: List[String], spaceCount: Int): List[String] = {
    if (words.length > 1) {
      val spaceEachChar = spaceCount / (words.length - 1)
      val pendingSpace = spaceCount - (spaceEachChar * (words.length - 1))
      val padding = List.fill(spaceEachChar)(" ").mkString("")
      val wordsWithPadding = words.init.map(_ + padding) ++ List(words.last)
      var i = 0
      wordsWithPadding.map { word =>
        if (i < pendingSpace) {
          i = i + 1
          word + " "
        } else word
      }
    } else words // one word needs no padding
  }

  def fullJustify(words: Array[String], maxWidth: Int): List[String] = {
    def  fullJustifyInternal(wordsList: List[String]): List[String] = {
      var charCount = 0
      // get words which can fit in one line
      val wordsInCurrentLine = wordsList.takeWhile { w =>
        if (charCount > maxWidth || charCount + w.length > maxWidth) false
        else {
            // add a space for each word
            charCount = charCount + 1 + w.length
            true
          }
      }

      val totalWordLegnth = wordsInCurrentLine.foldLeft(0){ (totalLength,w) =>  totalLength + w.length}

      val totalSpaceToDistribute = maxWidth - totalWordLegnth

      val finalCurrentLine = fillSpace(wordsInCurrentLine, totalSpaceToDistribute).mkString("")
      val pendingWords = wordsList.drop(wordsInCurrentLine.length)
      if (pendingWords.isEmpty) List(finalCurrentLine) else finalCurrentLine::fullJustifyInternal(pendingWords)
    }


    fullJustifyInternal(words.toList)
  }

  println(fullJustify(Array("This", "is", "an", "example", "of", "text", "justification."), 16).mkString("\n"))
  println()
  println(fullJustify(Array("What","must","be","acknowledgment","shall","be"), 16).mkString("\n"))
  println()
  println(fullJustify(Array("Science","is","what","we","understand","well","enough","to","explain","to","a","computer.",
                            "Art","is","everything","else","we","do"), 20).mkString("\n"))
}
