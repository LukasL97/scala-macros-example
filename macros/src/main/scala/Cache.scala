package de.codecentric

import scala.collection.mutable

trait Cache[K, V] {
  def put(key: K, value: V): Option[V]
  def get(key: K): Option[V]
}

class MapCache[K, V] extends Cache[K, V] {
  private val map = mutable.Map.empty[K, V]

  override def put(key: K, value: V): Option[V] = map.put(key, value)
  override def get(key: K): Option[V] = map.get(key)
}

object Cache {
  implicit val defaultCacheFactory: CacheFactory = new CacheFactory {
    override def apply[K, V](): Cache[K, V] = new MapCache[K, V]
  }
}

trait CacheFactory {
  def apply[K, V](): Cache[K, V]
}
