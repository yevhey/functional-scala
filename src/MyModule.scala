object MyModule {
  def factorial(n: Int): Int = {
    @scala.annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)
    go(n, 1)
  }

  def fib(n: Int): Int = {
    @scala.annotation.tailrec
    def go(prevSum: Int, sum: Int, index: Int): Int =
      if (n < index) sum
      else go(sum, prevSum + sum, index+1)

    go(0, 1, 2)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @scala.annotation.tailrec
    def loop(n: Int): Boolean =
      if (!ordered(as(n - 1), as(n))) false
      else if (n + 1 == as.length) true
      else loop(n + 1)

    loop(1)
  }

  def curry[A,B,C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def main(args: Array[String]): Unit = println(compose((x: Int) => x * 2, (y: Int) => y + 4)(4))
}
