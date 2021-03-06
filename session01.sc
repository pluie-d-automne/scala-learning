  1+3
  def abs(x:Double) = if (x<0) -x else x

  def sqrt(x: Double) = {
    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    sqrtIter(1.0)
  }
  sqrt(2)
  sqrt(4)
  sqrt(1e-6)
  sqrt(1e60)

  def factorial(n:Int):Int = {
    def loop(acc:Int, n:Int):Int =
      if(n==0) acc
        else loop(acc*n, n-1)
    loop(1,n)
  }
  factorial(4)