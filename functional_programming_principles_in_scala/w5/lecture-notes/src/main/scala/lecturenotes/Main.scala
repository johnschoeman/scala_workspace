object Main extends App {

  // 5.1
  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => Nil
    case y :: ys => y :: init(ys)
  }

  val l1 = List(1,2,4)
  // println(init(l1))

  def removeAt[T](n: Int, xs: List[T]) = {
    xs.take(n) ::: xs.drop(n + 1)
  }

  // println(removeAt(-1, List('a', 'b', 'c', 'd')))
  // println(removeAt(0, List('a', 'b', 'c', 'd')))
  // println(removeAt(1, List('a', 'b', 'c', 'd')))
  // println(removeAt(2, List('a', 'b', 'c', 'd')))
  // println(removeAt(3, List('a', 'b', 'c', 'd')))
  // println(removeAt(4, List('a', 'b', 'c', 'd')))

  def flatten(xs: List[Any]): List[Any] = {
    def loop(acc: List[Any], el: Any): List[Any] = {
      el match {
        case head :: tail => {
          loop(acc, head) ::: loop(acc, tail)
        }
        case List() => acc
        case _ => acc ::: List(el)
      }
    }

    loop(List(), xs)
  }

  val l2 = List(List(1,1, List(1,2,3)), 2, List(3, 4, List(5,8,9)))
  // println(flatten(l2))

  // 5.2 : Pairs and Tuples
  def merge(xs: List[Int], ys: List[Int]): List[Int] = {
    (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xtail, y :: ytail) => {
        if (x <= y) x :: merge(xtail, ys)
        else y :: merge(xs, ytail)
      }
    }
  }

  val l3 = List(1,3,5,7)
  val l4 = List(2,4,6,8)
  // println(merge(l3, l4))

  // 5.3 Implicit Parameters
  // val nums = List(2, -4, 5, 7, 1)
  // val strings = List("def", "abc", "jkl", "ghi")

  // Not paramaterized:
  object mergesort1 {
    def msort(xs: List[Int]): List[Int] = {
      val n = xs.length / 2
      if (n == 0) xs
      else {
        def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case(x :: xs1, y :: ys1) =>
            if (x < y) x :: merge(xs1, ys)
            else y :: merge(xs, ys1)
        }

        val (fst, snd) = xs splitAt n
        merge(msort(fst), msort(snd))
      }
    }
  }
  // println(mergesort1.msort(nums))

  // Parameterized
  object mergesort2 {
    def msort[T](xs: List[T])(lt: ((T, T) => Boolean)): List[T] = {
      val n = xs.length / 2
      if (n == 0) xs
      else {
        def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case(x :: xs1, y :: ys1) =>
            if (lt(x, y)) x :: merge(xs1, ys)
            else y :: merge(xs, ys1)
        }

        val (fst, snd) = xs splitAt n
        merge(msort(fst)(lt), msort(snd)(lt))
      }
    }
  }
  // println(mergesort2.msort(nums)((x: Int, y: Int) => x < y))
  // println(mergesort2.msort(nums)((x, y) => x < y))
  // println(mergesort2.msort(strings)((x: String, y: String) => x.compareTo(y) < 0))
  // println(mergesort2.msort(strings)((x, y) => x.compareTo(y) < 0))

  // Parameterized with Ordering
  import scala.math.Ordering
  object mergesort3 {
    def msort[T](xs: List[T])(ord: Ordering[T]): List[T] = {
      val n = xs.length / 2
      if (n == 0) xs
      else {
        def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case(x :: xs1, y :: ys1) =>
            if (ord.lt(x, y)) x :: merge(xs1, ys)
            else y :: merge(xs, ys1)
        }

        val (fst, snd) = xs splitAt n
        merge(msort(fst)(ord), msort(snd)(ord))
      }
    }
  }
  // println(mergesort3.msort(nums)(Ordering.Int))
  // println(mergesort3.msort(strings)(Ordering.String))

  // Parameterized with Implicit Ordering
  object mergesort4 {
    def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
      val n = xs.length / 2
      if (n == 0) xs
      else {
        def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case(x :: xs1, y :: ys1) =>
            if (ord.lt(x, y)) x :: merge(xs1, ys)
            else y :: merge(xs, ys1)
        }

        val (fst, snd) = xs splitAt n
        merge(msort(fst), msort(snd))
      }
    }
  }
  // println(mergesort4.msort(nums))
  // println(mergesort4.msort(strings))

  // 5.4 Higher-Order List Functions

  def squareList1(xs: List[Int]): List[Int] = {
    xs match {
      case Nil => Nil
      case y :: ys => (y * y) :: squareList1(ys)
    }
  }

  def squareList2(xs: List[Int]): List[Int] = {
    xs map (x => x * x)
  }

  val nums = List(1,2,3)
  // println(squareList1(nums))
  // println(squareList2(nums))

  // def pack[T](xs: List[T]): List[List[T]] = {
  //   xs match {
  //     case Nil => Nil
  //     case x :: xs1 => {
  //       val prev: List[List[T]] = pack(xs1)
  //       prev match {
  //         case Nil => List(List(x))
  //         case y :: ys1 => {
  //           if (y.contains(x)) {
  //             (x :: y) :: ys1
  //           } else {
  //             List(x) :: y :: ys1
  //           }
  //         }
  //       }
  //     }
  //   }
  // }

  def pack[T](xs: List[T]): List[List[T]] = {
    xs match {
      case Nil => Nil
      case x :: xs1 => {
        val (first, rest) = xs span (y => y == x)
        first :: pack(rest)
      }
    }
  }

  val list1 = List("a", "a", "a", "b", "c", "c", "a")
  val list2 = List("b", "a")
  val result1 = List(List("a", "a", "a"), List("b"), List("c", "c"), List("a"))
  // val res1 = pack(list1)
  // println("---")
  // println(res1)

  def encode[T](xs: List[T]): List[(T, Int)] = {
    pack(xs) map( ys => (ys(0), ys.length) )
  }

  // 5.5 Reduction of Lists
  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())( (el, acc) => f(el) :: acc )

  println(mapFun(List(1,2,3), (x: Int) => x * 2))

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)( (el, acc) => acc + 1 )

  println(lengthFun(List(1,2,3)))

}
