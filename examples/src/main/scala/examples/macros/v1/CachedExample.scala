package de.codecentric
package examples.macros.v1

import macros.v1.cached

object CachedExample extends App {

  val fCache = new MapCache[(Int, Int), Int]

  @cached
  def f(x: Int, y: Int): Int = x * y

  val gCache = new MapCache[(Int, String), String]

  @cached
  def g(x: Int, s: String): String = x.toString + s

  println(f(1, 2))
  println(f(1, 2))
  println(f(2, 1))

  println(g(3, "x"))
  println(g(3, "x"))
}
