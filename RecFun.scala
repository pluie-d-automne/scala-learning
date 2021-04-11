package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c==0 || c==r) 1 else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def par(chars: List[Char]):List[Char] = {
      if ((chars.head!='(' && chars.head !=')')&&chars.tail.isEmpty) Nil
      else if ((chars.head=='(' || chars.head ==')')&&chars.tail.isEmpty) chars.head::Nil
      else if (chars.head=='(' || chars.head ==')') chars.head::par(chars.tail)
      else par(chars.tail)
    }
    def first(x: List[Char]):Char = {
      if (x.isEmpty) ' '
      else if (x.head==')' || x.head =='(') x.head
      else first(x.tail)
    }
    def last(x: List[Char]):Char = {
      if ((x.head=='(' || x.head ==')') && x.tail.isEmpty) x.head
      //if (chars.tail.isEmpty && chars.head!='('&& chars.head!=')') ' '
      else last(x.tail)
    }
    def left(x: List[Char]):Int = {
      if (x.isEmpty) 0
      else if (x.head=='(' && x.tail.isEmpty) 1
      else if (x.head=='(') left(x.tail)+1
      else left(x.tail)
    }
    def right(x: List[Char]):Int = {
      if (x.isEmpty) 0
      else if (x.head==')' && x.tail.isEmpty) 1
      else if (x.head==')') right(x.tail)+1
      else right(x.tail)
    }
    left(par(chars)) == right(par(chars)) && first(par(chars)) == '(' && last(par(chars)) == ')'
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0 || coins.isEmpty) 0
    else if (money==coins.head) 1
    else if (money>coins.head) countChange(money-coins.head, coins)+countChange(money, coins.tail)
    else countChange(money, coins.tail)

  }
}
