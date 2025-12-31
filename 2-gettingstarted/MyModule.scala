object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  // exercise 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, current: Int, next: Int): Int =
      if (n <= 0) current
      else {
        go(n - 1, next, current + next)
      }

    go(n, 0, 1)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  // exercise 2.2
  def isSorted[A](as: Array[A], p: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (!p(as(n), as(n + 1))) false
      else go(n + 1)

    go(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)

  // exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  // exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  // specific format abs function
  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  // specific format factorial function
  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(n, factorial(n))
  }

  // generic format message value function
  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))

    println(formatResult("absolute value", -42, abs))
    println(findFirst(Array(7, 9, 13), (x: Int) => x == 9))
    println(isSorted(Array(7, 9, 13), (x: Int, y: Int) => x < y))
  }
}
