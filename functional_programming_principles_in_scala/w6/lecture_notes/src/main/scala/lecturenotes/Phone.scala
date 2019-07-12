package phone

object Phone {
  val words = List("scala", "is", "fun", "dvo", "I", "H", "like", "cheese")

  val mnem = Map(
    '2' -> "ABC",
    '3' -> "DEF",
    '4' -> "GHI",
    '5' -> "JKL",
    '6' -> "MNO",
    '7' -> "PQRS",
    '8' -> "TUV",
    '9' -> "WXYZ"
    )

  val charCode: Map[Char, Char] = {
    for {
      (digit, str) <- mnem
      ltr <- str
    } yield (ltr, digit)
  }

  def wordCode(word: String): String = {
    word.toUpperCase map(char => charCode(char))
  }

  val matchingWords: Map[String, Seq[String]] = {
    (words groupBy wordCode).withDefaultValue(Seq())
  }

  def translate(phoneNumber: String) = {
    def encode(number: String): Set[List[String]] = {
      if (number.isEmpty) Set(List())
      else {
        {
          for {
            index <- (1 to number.length)
            words <- encode(number drop index)
            potentialWord <- matchingWords(number take index)
          } yield {
            potentialWord :: words
          }
        }.toSet
      }
    }

    encode(phoneNumber).map(_.mkString(" "))
  }

  println(translate("7225247386"))
  println(translate("45453243373"))
  // println(translate("4"))
}
