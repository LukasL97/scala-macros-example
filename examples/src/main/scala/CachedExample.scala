package de.codecentric

object CachedExample extends App {
  @cached
  def f(x: Int, y: String): Int = x

  @cached
  def g(q: String): String = q

  println(f(1, "test"))
  println(f(1, "test"))
  println(f(2, "test"))
  println(f(2, "rest"))
  println(g("as"))
  println(g("as"))
}
