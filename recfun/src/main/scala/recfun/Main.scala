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
  def pascal(c: Int, r: Int): Int = {
    def pascal(numbers: List[Int], n: Int = 1) : Int = {
      if (n >= r) numbers(c)
      else pascal(1+:calculateInteriorNumbers(numbers):+1, n+1)
    }

    def calculateInteriorNumbers(row: List[Int]) = {
      row.sliding(2).map(_.sum).toList
    }

    pascal(List(1, 1))
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance(chars: List[Char], stack: List[Char]): Boolean = chars match {
      case Nil => stack.isEmpty
      case'('::t => balance(t, '('+:stack)
      case ')'::t if stack.isEmpty => false
      case ')'::t => balance(t, stack.tail)
      case _::t => balance(t, stack)
    }
    balance(chars, List())
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0) 0
    else if (money > 0 && coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
