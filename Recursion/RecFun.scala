package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
    //print(s"${countChange(4, List(1,2))} ")
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int) = {
    def factor(n: Int): Int ={
      if (n <= 0) 1
      else n * factor(n-1)
    }
    factor(r) / (factor(c) * factor(r-c))
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loopString(count: Int, str: List[Char]): Boolean =
      if (count < 0) false
      else if (str.isEmpty && count == 0) true
      else if (str.isEmpty) false
      else if (str.head == '(') loopString(count + 1, str.tail)
      else if (str.head == ')') loopString(count - 1, str.tail)
      else loopString(count, str.tail)

     loopString(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChange_(total: Int, money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty) total
      else if (money == 0) 1
      else {

        if (coins.head <= money) {
          countChange_(total + countChange_(0, money - coins.head, coins), money, coins.tail)
        } else {
          countChange_(total , money, coins.tail)
        }
      }
    }
    countChange_(0, money, coins)
  }
}
