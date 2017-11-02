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
      def factorial(n: Int): Int = {
        def loop(accu: Int,n: Int): Int = {
          if(n==0) accu
          else loop(accu*n,n-1)
        }
        loop(1,n)
      }
      factorial(r)/(factorial(c)*factorial(r-c))
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanced(chars: List[Char], count: Int): Boolean = {
        if(chars.isEmpty) count == 0
        else if(chars.head == '(') balanced(chars.tail,count+1)
        else if(chars.head == ')') count > 0 && balanced(chars.tail,count-1)
        else balanced(chars.tail,count)
      }
      balanced(chars,0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def count(money: Int,coins: List[Int]): Int = {
        if(coins.isEmpty || money == 0) 0
        else if (money - coins.head == 0) 1
        else if (money - coins.head < 0) 0
        else countChange(money - coins.head,coins) + countChange(money,coins.tail)
      }
      count(money,coins.sorted)
    }
  }
