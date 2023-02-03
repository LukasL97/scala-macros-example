package de.codecentric
package examples.macros.v3

import macros.v3.cached

import com.google.common.cache.CacheBuilder

import java.util.concurrent.TimeUnit

object CachedExample extends App {

  implicit object GuavaCacheFactory extends CacheFactory {
    override def apply[K, V](): Cache[K, V] = new GuavaCache[K, V]
  }

  @cached
  def f(x: Int, y: Int): Int = x * y

  @cached
  def g(x: Int, s: String): String = x.toString + s

  println(f(1, 2))
  println(f(1, 2))
  println(f(2, 1))

  println(g(3, "x"))
  println(g(3, "x"))
}

class GuavaCache[K, V] extends Cache[K, V] {

  println("Initialize GuavaCache")

  private val cache = CacheBuilder.newBuilder()
    .maximumSize(1000)
    .expireAfterAccess(5, TimeUnit.MINUTES)
    .build[K, V]

  override def put(key: K, value: V): Option[V] = {
    val oldValue = get(key)
    cache.put(key, value)
    oldValue
  }

  override def get(key: K): Option[V] = Option(cache.getIfPresent(key))
}
