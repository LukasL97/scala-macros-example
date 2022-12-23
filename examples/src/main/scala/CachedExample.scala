package de.codecentric

object CachedExample extends App {

  val cache = new MapCache[(Int, String), Int]

  @cached
  def f(x: Int, y: String): Int = x

  println(f(1, "test"))
  println(f(1, "test"))
  println(f(2, "test"))
  println(f(2, "rest"))

}
