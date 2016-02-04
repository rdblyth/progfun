package recfun
import common._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack

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
    def pascal(numbers: List[Int], n: Int) : Int = {
      if (n == r) numbers(c)
      else pascal(1+:numbers.sliding(2).map(_.sum).toList:+1, n+1)
    }
    if (r < 2) 1
    else pascal(List(1, 1), 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance(chars: List[Char], stack: Stack[Char]): Boolean = {
      if (chars.isEmpty) return stack.isEmpty

      if (chars.head == '(') {
        stack.push('(');
      } else if (chars.head == ')') {
        if (stack.isEmpty) return false
        stack.pop()
      }
      balance(chars.tail, stack);

    }
    balance(chars, Stack[Char]());
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
