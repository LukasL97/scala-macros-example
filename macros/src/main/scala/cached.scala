package de.codecentric

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox


class cached extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro cached.impl
}

object cached {
  def impl(c: whitebox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    annottees.head match {
      case q"$mods def $method[..$typeParams](...$params): $returnType = $rhs" =>
        val flattenedParams = params.asInstanceOf[Seq[Seq[c.Tree]]].flatten
        val paramNames = flattenedParams.map {
          case q"$_ val $name: $_ = $_" => name
        }
        val newRhs =
          q"""
           val key = (..$paramNames)
           return cache.get(key) match {
             case Some(value) =>
               println("CACHE HIT")
               value
             case None =>
               println("CACHE MISS")
               val result = $rhs
               cache.put(key, result)
               return result
           }
          """
        q"$mods def $method[..$typeParams](...$params): $returnType = $newRhs"
      case annottee => c.abort(annottee.pos, "Annottee must be a method")
    }
  }
}
