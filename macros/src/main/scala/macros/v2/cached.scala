package de.codecentric
package macros.v2

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
        val (paramNames, paramTypes) = flattenedParams.map {
          case q"$_ val $name: $tpt = $_" => name -> tpt
        }.unzip

        val cacheName = TermName(c.freshName(method + "_generatedCache"))
        val keyName = TermName(c.freshName("key"))
        val resultName = TermName(c.freshName("result"))

        val newRhs =
          q"""
           val $keyName = (..$paramNames)
           $cacheName.get($keyName) match {
             case Some(value) =>
               println("CACHE HIT for key " + $keyName + ": " + value)
               value
             case None =>
               println("CACHE MISS for key " + $keyName)
               val $resultName = $rhs
               $cacheName.put($keyName, $resultName)
               $resultName
           }
          """
        val expandedMethod = q"$mods def $method[..$typeParams](...$params): $returnType = $newRhs"

        val cacheKeyType = tq"(..$paramTypes)"
        val cache = q"private val $cacheName: de.codecentric.Cache[$cacheKeyType, $returnType] = new de.codecentric.MapCache[$cacheKeyType, $returnType]"

        q"$expandedMethod; $cache"
      case annottee => c.abort(annottee.pos, "Annottee must be a method")
    }
  }
}
