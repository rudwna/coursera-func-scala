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
    if (c == 0 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def process(chars: List[Char], openedParens: Int, unclosed: Boolean): Boolean = {
      if (chars.isEmpty) {
        openedParens == 0 && !unclosed
      } else {
        val isUnclosed = unclosed || openedParens == 0 && chars.head == ')'
        process(chars.tail, if (chars.head == '(') openedParens + 1 else if (chars.head == ')') openedParens - 1 else openedParens, isUnclosed)
      }
    }

    process(chars, openedParens = 0, unclosed = false)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(money: Int, coins: List[Int], prevCoinUsed: Int): Int =
      if (money == 0) 1 else if (coins.isEmpty) 0
      else
        coins.map(coin => if (coin <= money && prevCoinUsed <= coin) count(money - coin, coins, coin) else 0).sum

    count(money, coins, prevCoinUsed = -1)
  }
}
