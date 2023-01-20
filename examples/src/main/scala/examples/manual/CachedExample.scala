package de.codecentric
package examples.manual

object CachedExample extends App {

  def cached[K, V](cache: Cache[K, V], input: K)(f: => V): V = {
    cache.get(input) match {
      case Some(value) =>
        println(s"CACHE HIT for key $input: $value")
        value
      case None =>
        println(s"CACHE MISS for key $input")
        val result = f
        cache.put(input, result)
        result
    }
  }

  val fCache = new MapCache[(Int, Int), Int]

  def f(x: Int, y: Int): Int = cached(fCache, (x, y)) {
    x * y
  }

  val gCache = new MapCache[(Int, String), String]

  def g(x: Int, s: String): String = cached(gCache, (x, s)) {
    x.toString + s
  }

  println(f(1, 2))
  println(f(1, 2))
  println(f(2, 1))

  println(g(3, "x"))
  println(g(3, "x"))
}
