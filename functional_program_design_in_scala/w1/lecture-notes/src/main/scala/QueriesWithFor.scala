package querieswithfor

class QueriesWithFor {
  case class Book(title: String, authors: List[String])

  val books: List[Book] = List(
    Book("A", List("Bob", "Alice")),
    Book("B", List("Alice")),
    Book("B but more B", List("Alice")),
    Book("B before C was cool", List("Alice")),
    Book("C Programming", List("Bob")),
    Book("Programming Scala", List("Martin"))
  )

  val booksSet = books.toSet

  // find the titles of the books whose authors name is Bob
  val a = for {
    b1 <- books
    a1 <- b1.authors
    if a1 == "Bob"
  } yield b1.title

  // println(a)

  // find all the books whose names have the word Program in them
  val b = for {
    b1 <- books
    if b1.title.indexOf("Program") != -1
  } yield b1

  // println(b)

  // find the names of all authors who have written at least 2 books
  val c = { for {
    b1 <- books
    b2 <- books
    if b1.title < b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1 }.distinct

  val d = for {
    b1 <- booksSet
    b2 <- booksSet
    if b1.title < b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1

  println(d)
}
