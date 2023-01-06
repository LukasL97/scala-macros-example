package de.codecentric

object CachedExample extends App {

  @cached
  def f(x: Int, y: String): Int = x

  @cached
  def g(x: Int, y: String): Int = x + 1

  @cached
  def g(x: Int): Int = x + 1

  println(f(1, "test"))
  println(f(1, "test"))
  println(f(2, "test"))
  println(f(2, "rest"))
  println(g(2, "rest"))
  println(g(2, "rest"))
  println(g(2, "rest"))

}
