object w6 extends App {
  // 6.1 Other Collections
  val xs = Array(1,2,3,4)
  // println(xs map (x => x +1))

  val s = "Hello World"
  // println(s filter (c => c.isUpper))
  //

  // println(s exists (c => c.isUpper))
  // println(s forall (c => c.isUpper))
  // val y = List(1,2,3).zip(s)
  // println(y)
  // println(y.unzip)
  // println(s.flatMap (c => List('.', c)))
  // println(xs.sum)
  // println(xs.max)

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double = {
    (xs.zip(ys)).map(xy => xy._1 * xy._2).sum
  }

  def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double = {
    (xs.zip(ys)).map{ case (x, y) => x * y }.sum
  }

  // println(scalarProduct2(Vector(1.0,2.0),Vector(3.0,4.0)))

  def isPrime(n: Int): Boolean = (2 until n).forall(d => n % d != 0)

  // println(isPrime(140))

  // 6.2 Combinatorial Search and For-Expressions
  def triangleNumbers(lowerBound: Int, upperBound: Int) = {
    (lowerBound to upperBound).flatMap(x => (x to upperBound).map(y => (x, y)))
  }
  // println(triangleNumbers(1, 10))


  case class Person(name: String, age: Int)

  def forComp1(persons: List[Person]) = {
    for (p <- persons if p.age > 20) yield p.name
  }
  def forComp2(persons: List[Person]) = {
    persons filter (p => p.age > 20) map (p => p.name)
  }
  val persons: List[Person] = List(new Person("a", 19), new Person("b", 20), new Person("c", 21))
  println(forComp1(persons))
  println(forComp2(persons))

  val n = 7
  def primePairs(n: Int) = {
    (1 until n).flatMap(i =>
        (1 until i).map(j => (i,j))) filter (pair =>
            isPrime(pair._1 + pair._2))
  }
  def primePairs2(n: Int) = {
    for {
      i <- (1 until n)
      j <- (1 until i)
      if isPrime(i + j)
    } yield (i, j)
  }
  println(primePairs(n))
  println(primePairs2(n))


  def scalarProductComp(xs: Vector[Double], ys: Vector[Double]): Double = {
    (for {
      x <- xs
      y <- ys
    } yield (x + y)).sum
  }

  println(scalarProductComp(Vector(1.0,2.0),Vector(3.0,4.0)))

  // 6.3 Combinatorial Search Example

  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else {
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
      }
    }

    def isSafe(col: Int, queens: List[Int]): Boolean = {
      val row = queens.length
      val queensWithRow = (row - 1 to 0 by -1) zip queens
      queensWithRow forall {
        case (r, c) => c != col && Math.abs(col - c) != row - r
      }
    }

    placeQueens(n)
  }

  def show(queens: List[Int]) = {
    val lines =
      for (col <- queens.reverse)
      yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
    "\n" + (lines mkString "\n")
  }

  // println(queens(8) take 3 map show mkString "\n")

  // 6.4 Maps
  val romanNumerals: Map[String, Int] = Map("I" -> 1, "V" -> 5, "X" -> 10)
  val capitals: Map[String, String] = Map("Washington" -> "US")
  // println(capitals)
  // println(capitals("Washington"))
  // println(capitals get "Washington") // Some("US")
  // println(capitals get "androda")  // None

  def showCountry(capital: String): String = {
    capitals get capital match {
      case Some(x) => x
      case None => "Missing Data"
    }
  }

  // println(showCountry("Washington"))
  // println(showCountry("andorda"))

  // options are collections and can be used in for-comprehensions
  case class Person2(first: String, last: String)

  val maybeFirst = Some("alice")
  val maybeLast1 = None
  val maybeLast2 = Some("smith")

  // println(for { first <- maybeFirst; last <- maybeLast1 } yield Person2(first, last))
  // println(for { first <- maybeFirst; last <- maybeLast2 } yield Person2(first, last))

  val fruit = List("apple", "pear", "orange", "pineapple")
  // println(fruit sortWith (_.length < _.length))
  // println(fruit.sorted)
  // println(fruit groupBy (_.head))

  // without defaultValue
  // class Poly(val terms: Map[Int, Double]) {
  //   def + (other: Poly): Poly = {
  //     new Poly(terms ++ (other.terms map adjust))
  //   }
  //   def adjust(term: (Int, Double)): (Int, Double) = {
  //     val (exp, coeff) = term
  //     terms get exp match {
  //       case Some(coeff1) => exp -> (coeff + coeff1)
  //       case None => exp -> coeff
  //     }
  //   }
  //
  //   override def toString: String = {
  //     (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
  //   }
  // }

  class Poly(terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    def terms = terms0 withDefaultValue(0.0)
    // def + (other: Poly): Poly = {
    //   new Poly(terms ++ (other.terms map adjust))
    // }
    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      // (exp, terms(exp) + coeff)
      exp -> (coeff + terms(exp))
    }

    def + (other: Poly): Poly = {
      new Poly((other.terms foldLeft terms)(addTerm))
    }
    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      val (exp, coeff) = term
      terms + (exp -> (coeff + terms(exp)))
    }


    override def toString: String = {
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
    }
  }

  val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
  val p2 = new Poly(0 -> 3.0, 3 -> 7.0)

  println(p1 + p2)
}
