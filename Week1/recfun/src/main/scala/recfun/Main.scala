package recfun
import common._

object Main {
  def main(args: Array[String]) {
//    println("Pascal's Triangle")
//    for (row <- 0 to 10) {
//      for (col <- 0 to row)
//        print(pascal(col, row) + " ")
//      println()
//    }
  convertBinary(8)
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if(c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(chars: List[Char], count: Int): Boolean = {
      if (chars.isEmpty && count == 0)
        true
      else if (chars.isEmpty || count != 0)
        false
      else if (chars.head == '(')
        loop(chars.tail, count + 1)
      else if (chars.head == ')')
        loop(chars.tail, count - 1)
      else
        loop(chars.tail, count)
    }
    loop(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }

  def reverseString(string: String): String = {
    def loop(acc: String, original: String): String = {
      if(original == "") acc
      else loop(original.head + acc, original.tail)
    }
    loop("", string)
  }

  def removeDuplicates(string: String): String = {
    if (string.length == 1) string
    else if (string.charAt(0).equals(string.charAt(1)))
      removeDuplicates(string.tail)
    else string.head + removeDuplicates(string.tail)
  }

  def modulus(value: Int, divisor: Int): Int = {
    if (value - divisor < 0) value
    else if(value == 0) 0
    else modulus(value - divisor, divisor)
  }

  def convertBinary(number: Int) {
    if(number < 2) {
      print(number)
    }
    else {
      convertBinary(number / 2)
      print(number % 2)
    }
  }
}
