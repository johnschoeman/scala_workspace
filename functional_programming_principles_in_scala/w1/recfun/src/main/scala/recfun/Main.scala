package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int =
      if (c > r) 0
      else if (c < 0) 0
      else if (r == 0) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean =
      balanceLoop(0, chars)

    def balanceLoop(acc: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) {
        (acc == 0)
      } else if (acc < 0) {
        false
      } else {
        if (chars.head == '(')
          balanceLoop(acc + 1, chars.tail)
        else if (chars.head == ')')
          balanceLoop(acc - 1, chars.tail)
        else
          balanceLoop(acc, chars.tail)
      }
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      val sortedCoins = coins.sortWith(_ > _)
      countChangeLoop(0, money, sortedCoins)
    }

    def countChangeLoop(acc: Int, money: Int, coins: List[Int]): Int =
      if (money == 0) acc
      else if (coins.isEmpty) acc
      else {
        val head = coins.head
        val diff = money - head
        if (diff == 0) {
          countChangeLoop(acc + 1, 0, coins) + countChangeLoop(acc, money, coins.tail)
        } else if (diff > 0) {
          countChangeLoop(acc, diff, coins) + countChangeLoop(acc, money, coins.tail)
        } else {
          countChangeLoop(acc, money, coins.tail)
        }
      }
  }
