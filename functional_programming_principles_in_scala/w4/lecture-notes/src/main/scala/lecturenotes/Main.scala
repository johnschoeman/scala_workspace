
object Main extends App {
  // import natural._
  // import Boolean._
  // import mylist.{MyList}
  // import variance._
  import decomp._

  // False.ifThenElse(println('true'), println('false'))
  // println(True && False)
  // println(True || True)
  // println(False.unary_!)
  // println(!(!True))
  // println(True == True)
  // println(True != True)
  // println(True != False)
  // println(False != True)
  // println(False != False)
  // println('-------')
  // println(True < True)
  // println(True < False)
  // println(False < True)
  // println(False < False)

  // println(Zero)
  // val one = Zero.successor
  // println(one)
  // val two = one.successor
  // println(two)
  // val four = two.successor.successor
  // println(four)
  // val three = four.predecessor
  // println(three)
  // println(three + two)
  // println(two - one)

  // println(MyList())
  // println(MyList(1))
  // println(MyList(2,3))

  // val a: List[Int] = new Cons(3, new Cons(2, new Cons(1, Nil)))
  // val b: List[String] = new Cons('a', new Cons('b', Nil))
  // val c: List[Any] = a prepend 'a'
  // println(a)
  // println(b)
  // print(c)

  // val a: Number = new Number(2)
  // val b: Sum = new Sum(a, new Number(40))
  // val c: Sum = new Sum(b, new Sum(b, a))
  // val d: Prod = new Prod(new Number(1), a)
  // println(a.show(a))
  // println(b.eval(b))
  // println(b.show(b))
  // println(c.eval(c))
  // println(d.show(d))
  // println(d.eval(d))
  // val x: Sum = Sum(Prod(Number(2), Var("x")), Var("y"))
  // val y: Prod = Prod(Sum(Number(2), Var("x")), Var("y"))
  // println(x.show(x))
  // println(y.show(y))

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => {
      if (x < y) x :: y :: ys else y :: insert(x, ys)
    }
  }

  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  }

  val a: List[Int] = List(1,3,5)
  val b: List[Int] = insert(4, a)
  println(b)
  val c: List[Int] = List(2,6,4,1,5,3)
  val d: List[Int] = isort(c)
  println(d)
}

