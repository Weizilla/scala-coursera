package recfun
import common._

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
  def pascal(c: Int, r: Int): Int = {
    if (c > r) {
      0
    }
    if (c == 0 || r == 0 || c == r) {
      1
    }
    else {
      pascal(c , r - 1) + pascal(c - 1, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance(depth: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) {
        depth == 0
      } else {
        if (chars.head == '(') {
          balance(depth + 1, chars.tail)
        } else if (chars.head == ')') {
          if (depth == 0) {
            false
          } else {
            balance(depth - 1, chars.tail)
          }
        } else {
          balance(depth, chars.tail)
        }
      }
    }
    balance(0, chars)
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) {
      1
    } else if (money < 0) {
      0
    } else {
      var sum = 0
      for (c <- coins) {
        sum += countChange(money - c, coins)
      }
      sum
    }
  }
}
