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
    def pascal(row: List[Int]): Int = {
      if (row.size - 1 == r) row(c)
      else pascal(calculateNextRow(row))
    }
    pascal(List(1))
  }

  def calculateNextRow(row: List[Int]): List[Int] = {
    def calculateNextRow(row: List[Int], nextRow: List[Int]): List[Int] = {
      if (row.size == 1) nextRow :+ 1
      else calculateNextRow(row.tail, nextRow :+ row(0) + row(1))
    }

    calculateNextRow(row, List(1))
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
