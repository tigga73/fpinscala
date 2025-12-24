object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n 
    else n 

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, current: Int, next: Int): Int = 
      if (n <= 0) current
      else {
        go(n-1, next, current+next)
      }
    
    go(n, 0, 1)
  }

  def fib2(n: Int): List[Int] = {
    @annotation.tailrec
    def go(n: Int, current: Int, next: Int, acc: List[Int]): List[Int] = 
      if (n <= 0) acc.reverse
      else go(n-1, next, current+next, current :: acc)
      
    
    go(n, 0, 1, List.empty)
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
    println(fib(7))
    println(fib2(7))
}
