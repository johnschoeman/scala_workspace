package phone

object Phone {
  val words = List("SCALA", "IS", "FUN", "DVO", "I", "H", "LIKE", "CHEESE")

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

    val charCode: Map[Char, Char] = Map(
      'A' -> '2', 'B' -> '2', 'C' -> '2',
      'D' -> '3', 'E' -> '3', 'F' -> '3',
      'G' -> '4', 'H' -> '4', 'I' -> '4',
      'J' -> '5', 'K' -> '5', 'L' -> '5',
      'M' -> '6', 'N' -> '6', 'O' -> '6',
      'P' -> '7', 'Q' -> '7', 'R' -> '7',
      'S' -> '7',
      'T' -> '8', 'U' -> '8', 'V' -> '8',
      'W' -> '9', 'X' -> '9', 'Y' -> '9',
      'Z' -> '9'
    )

  def translate(phoneNumber: String) = {

    def translateNext(number: String): Set[List[String]] = {
      if (number.length == 0) {
        Set(List())
      }
      else {
        for {
          index <- (1 to number.length).toSet map((i: Int) => List(i.toString))
          words <- translateNext(number drop index.head.toInt)
          val subNumber = number take index.head.toInt
          potentialWord <- matchingWords(subNumber)
        } yield {
          potentialWord :: words
        }
      }
    }

    translateNext(phoneNumber)
  }

  def wordCode(word: String): String = {
    word map(char => charCode(char))
  }

  def matchingWords(number: String): List[String] = {
    words filter(word => wordCode(word) == number)
  }

  println(translate("7225247386"))
  println(translate("45453243373"))
  // println(translate("4"))
}
