package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if c == 0 || c == r then 1
    else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    def balancer(chars: List[Char], parCount: Int = 0): Boolean =
      if chars.isEmpty then parCount==0
      else if chars.head == '(' then balancer(chars.tail, parCount+1)
      else if chars.head == ')' && parCount < 1 then false
      else if chars.head == ')' then balancer(chars.tail, parCount-1)
      else balancer(chars.tail, parCount)
    balancer(chars)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if money <=0 || coins.isEmpty then 0
    else if money ==coins.sorted.head then 1
    else if money < coins.head then countChange(money, coins.tail.sorted)
    else countChange(money-coins.sorted.head, coins.sorted) + countChange(money, coins.tail)
