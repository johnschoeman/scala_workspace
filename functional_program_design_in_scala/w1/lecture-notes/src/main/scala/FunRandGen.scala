package funrandgen

class FunRandGen {
  // trait Generator[+T] {
  //   def generate: T
  // }

  val integers = new Generator[Int] {
    val rand = new java.util.Random
    def generate = rand.nextInt()
  }

  // val booleans = new Generator[Boolean] {
  //   def generate = integers.generate > 0
  // }

  // val pairs = new Generator[(Int, Int)] {
  //   def generate = (integers.generate, integers.generate)
  // }

  trait Generator[+T] {
    self => // alias for this

    def generate: T

    def map[S](f: T => S): Generator[S] = new Generator[S] {
      def generate = f(self.generate)
    }

    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      def generate = f(self.generate).generate
    }
  }

  val booleans = for (x <- integers) yield x > 0

  def pairs[T, U](t: Generator[T], u: Generator[U]) = for {
    x <- t
    y <- u
  } yield (x, y)

  def single[T](x: T): Generator[T] = new Generator[T] {
    def generate = x
  }

  def choose(lo: Int, hi: Int): Generator[Int] =
    for (x <- integers) yield lo + x % (hi - lo)

  def oneOf[T](xs: T*): Generator[T] =
    for (idx <- choose(0, xs.length)) yield xs(idx)

  // println(pairs(integers, booleans).generate)
  // println(single(1).generate)
  // println(choose(1,10).generate)
  // println(oneOf("waht", "okay").generate)


  def lists: Generator[List[Int]] = for {
    isEmpty <- booleans
    list <- if (isEmpty) emptyList else nonEmptyList
  } yield list

  def emptyList: Generator[List[Int]] = single(Nil)

  def nonEmptyList: Generator[List[Int]] = for {
    head <- integers
    tail <- lists
  } yield head :: tail

  println(lists.generate)

  trait Tree

  case class Inner(left: Tree, right: Tree) extends Tree

  case class Leaf(x: Int) extends Tree

  def trees: Generator[Tree] = for {
    isLeaf <- booleans
    tree <- if (isLeaf) leafNode else innerTree
  } yield tree

  def leafNode: Generator[Tree] = for {
    integer <- integers
  } yield Leaf(integer)

  def innerTree: Generator[Tree] = for {
    left <- trees
    right <- trees
  } yield Inner(left, right)

  println(trees.generate)

  def test[T](g: Generator[T], numTimes: Int = 1000)(test: T => Boolean): Unit = {
    for (i <- 1 to numTimes) {
      val value = g.generate
      assert(test(value), "test failed for: " + value)
    }
    println("passed " + numTimes + " tests.")
  }

  def myFun(x: Int): Int = x + 1

  def myTest(x: Int): Boolean = {
    // myFun(x) > 100000
    true
  }

  test(integers)(myTest)

  test(pairs(lists, lists)) {
    case (xs, ys) => (xs ++ ys).length >= xs.length
  }
}
