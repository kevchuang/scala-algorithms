package strings

object StringsAlgorithms {

  def isUnique(str: String): Boolean = {
    val maxAscii = 128
    val characterCounts: Array[Option[Boolean]] = (for (_ <- 0 to maxAscii) yield None).toArray
    for (i <- 0 until str.length) {
      if (characterCounts(str(i)).isDefined)
        return false
      else
        characterCounts(str(i)) = Some(true)
    }
    true
  }

  def checkPermutation(firstStr: String, secondStr: String): Boolean = {
    if (firstStr.length != secondStr.length)
      false
    else {
      val length = firstStr.length - 1
      for (i <- 0 until firstStr.length) {
        if (firstStr(i) != secondStr(length - i))
          return false
      }
      true
    }
  }

  def isPalindromePermutation(str: String): Boolean = {
    val charactersCount = str.foldLeft(Map.empty[Char, Int])((acc, character) =>
      if (character.isLetter)
        acc + ((character.toLower, acc.getOrElse(character.toLower, 0) + 1))
      else
        acc
    )
    val oddCount = charactersCount.foldLeft(0)((acc, character) => if (character._2 % 2 != 0) acc + 1 else acc)
    oddCount <= 1
  }

  def isPalindromePermutationOptimized(str: String): Boolean = {
    val oddCount = str.foldLeft((0, Map.empty[Char, Int]))((acc, character) =>
      if (character.isLetter) {
        val lowerCharacter = character.toLower
        val countCharacter = acc._2.getOrElse(lowerCharacter, 0) + 1
        val oddCount = if (countCharacter % 2 != 0) acc._1 + 1 else acc._1 - (if (acc._1 == 0) 0 else 1)
        (oddCount, acc._2 + ((lowerCharacter, countCharacter)))
      } else {
        (acc._1, acc._2)
      }
    )._1
    oddCount <= 1
  }
}
