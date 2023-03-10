# Compile-time code generation with Scala macro annotations

In this blog post we will take a look at macro annotations, a powerful tool for code transformation and generation in Scala.
Macro annotations allow us to transform the code of a definition, e.g., a class or method, at compile time.
This can be used to reduce boilerplate code, because the compiler generates it for us.
During compilation, a macro annotation will expand the *abstract syntax tree (AST)* of the annotated definition.
The transformation code is written in Scala itself, making use of Scala's reflection API to analyze and transform the AST.
Hence, the use of macros is also referred to as *metaprogramming*.

Macro *annotations* are one of two types of macros in Scala 2. 
The other one are *def macros*, which are explained in [[1]](https://docs.scala-lang.org/overviews/macros/overview.html).
Macro annotations are an experimental feature and have to be enabled via compiler flag `-Ymacro-annotations`
in Scala 2.13, or by using the macro paradise plugin in older version starting from Scala 2.10.

With the rework of the language in Scala 3, macro annotations have unfortunately been removed from the language.
Only recently they have been added again as an experimental feature in the pre-release version 
[3.3.0-RC2](https://github.com/lampepfl/dotty/releases/tag/3.3.0-RC2).
Compared to Scala 2, their usage is still rather clumsy.
Hence, we will first have a look at macro annotations in Scala 2 and afterward compare the example
to a (mostly) equivalent implementation in Scala 3.

## Caching method return values with macro annotations in Scala 2

As a simple example for macros in Scala 2 (version 2.13.10), we will implement a macro annotation for caching return values of arbitrary methods.
The cache will store key-value pairs of the input parameters of the method as well as its return value
and directly return them for cached inputs.
The code we will see in the following can be found [here](https://github.com/LukasL97/scala-macros-example).

We start by defining a `Cache` trait with a simple `MapCache` implementation:

```scala
trait Cache[K, V] {
  def put(key: K, value: V): Option[V]
  def get(key: K): Option[V]
}

class MapCache[K, V] extends Cache[K, V] {
  private val map = mutable.Map.empty[K, V]

  override def put(key: K, value: V): Option[V] = map.put(key, value)
  override def get(key: K): Option[V] = map.get(key)
}
```

Before introducing the macro annotation, we first want to have a look at how an implementation *without macros* could look like.
The caching logic can be generalized into a method with type parameters `K` and `V` for the key and value types:

```scala
def cached[K, V](cache: Cache[K, V], input: K)(f: => V): V = {
  cache.get(input) match {
    case Some(value) =>
      value
    case None =>
      val result = f
      cache.put(input, result)
      result
  }
}
```

The method takes the `cache` and the `input` as parameters and wraps a function `f`. 
It looks up the `input` in the `cache` and returns the respective value if one is found.
In case of a cache miss, the function `f` is executed and the result is cached and then returned.

This `cached` method can now be used in methods of arbitrary signature:

```scala
val fCache = new MapCache[(Int, Int), Int]

def f(x: Int, y: Int): Int = cached(fCache, (x, y)) {
  x * y
}

val gCache = new MapCache[(Int, String), String]

def g(x: Int, s: String): String = cached(gCache, (x, s)) {
  x.toString + s
}
```

The generic `cached` method provides some simplification for implementing the methods whose results we want to cache.
However, we still have to define a cache for each method and pass it along with the input parameters to the `cached` method.
This feels like boilerplate, which we can get rid of with a macro annotation.

### Generating the caching logic

We introduce a `cached` macro annotation, which can be used to annotate a method of arbitrary signature, providing all
the required caching logic.
In the first step, we only implement the transformation of the annotated method itself. 
For now, the cache that is used by the annotated method still has to be defined in the application code. 
This will be improved in a later step.

```scala
class cached extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro cached.impl
}

object cached {
  def impl(c: whitebox.Context)(annottees: c.Tree*): c.Tree = ???
}
```

The `cached` annotation extends the `StaticAnnotation` class and defines a `macroTransform` method.
The actual code transformation happens in the `impl` method, which gets the *annottees* along with a context as inputs.

The *annottees* are a sequence of ASTs, namely the definitions that are annotated by the `cached` annotation.
In our example, there will always be one annottee, as we will annotate methods. 
Multiple annottees can occur, for instance, when a class with a companion object is annotated.
In this case, both the class and its companion object are passed to the `macroTransform` method and can be transformed
in its implementation.

The context provides access to Scala's reflection API, including features such as *quasiquotes* and *term names*, which we
will make use of in the transformation code. We can import those directly as `import c.universe._`.

Now, let's have a look at the implementation of the macro transformation. 
This may seem overwhelming at first, but we will give a step-by-step explanation below:

```scala
def impl(c: whitebox.Context)(annottees: c.Tree*): c.Tree = {
  import c.universe._

  annottees.head match {
    case q"$mods def $method[..$typeParams](...$params): $returnType = $rhs" =>        // (1)

      val paramNames = params.flatten.map {                                            
        case q"$_ val $name: $_ = $_" => name                                          // (2)
      }

      val cacheName = TermName(s"${method}Cache")                                      // (3)
      val keyName = TermName(c.freshName("key"))
      val resultName = TermName(c.freshName("result"))

      val newRhs =                                                                     // (4)
        q"""
         val $keyName = (..$paramNames)
         $cacheName.get($keyName) match {
           case Some(value) =>
             value
           case None =>
             val $resultName = $rhs
             $cacheName.put($keyName, $resultName)
             $resultName
         }
        """
      val expandedMethod =                                                             // (5)
        q"$mods def $method[..$typeParams](...$params): $returnType = $newRhs"

      expandedMethod

    case annottee => c.abort(annottee.pos, "Annottee must be a method")                // (6)
  }
}
```

We start in (1) by pattern matching the annottee against a quasiquote, a code snippet wrapped inside an interpolated string `q"..."`.
Quasiquotes are a useful notation for constructing ASTs, which would otherwise have to be written as a bunch of nested
method applications.
As seen in the code, quasiquotes can also be used for pattern matching, allowing us to extract parts of the AST.
Conceptually, this is similar to normal string interpolation with `s"..."`, which allows inserting or extracting parts
of the string.
Note that in quasiquotes, we extract parts of the AST instead of strings.
For instance, `$mods` are the modifiers of the annotated method (e.g. `private`) and `rhs` is the expression on the
right-hand side of the annottee. 
Also note how the parameters and type parameters of the method are extracted.
The `..$typeParams` notation indicates a list of trees (in this case type trees).
Meanwhile, the `...$params` notation indicates a nested list of lists of trees, representing the parameter lists
of the annotated method.

The pattern matching ensures, that the annottee is indeed a method.
Otherwise, we jump to the default case (6), where we abort compilation with a corresponding error message.

In (2), we extract the names from the parameters, which we will use in the transformed right-hand side of the method
to build the key that is looked up and stored in the cache. 
Then, we define some variable names in (3), which we will also use in the right-hand side.
The `$cacheName` references a variable outside the method scope, which we assume to be present.
If it is not, the compilation will fail.
The other two variable names will reference local variables in the method body.
To avoid any naming conflicts with existing variables, we create them calling the `freshName` method from Scala's
reflection API, which ensures that the generated name will be unique.

The main part of the code transformation happens in (4), where we define `newRhs`, the new body of the method.
The new right-hand side is more or less identical to the implementation of the `cached` method that we defined earlier.
The key is created as a tuple built from the method parameters, using the `(..paramNames)` notation.
In case of a cache miss, the original method body `rhs` is used to retrieve a result.

Finally, in (5), the `expandedMethod` is built, using all the parts of the original method, but replacing the right-hand side
with `newRhs`. This method definition will replace the annotated method during compile time.

With this in place, we can annotate arbitrary methods as `@cached`. 
The compiler will then transform each method according to the macro transformation defined above.
The application code now looks as follows, still requiring the manual definition of the `fCache` and `gCache` for
the two methods `f` and `g`, but already the method definitions themselves are simplified:

```scala
val fCache = new MapCache[(Int, Int), Int]

@cached
def f(x: Int, y: Int): Int = x * y

val gCache = new MapCache[(Int, String), String]

@cached
def g(x: Int, s: String): String = x.toString + s
```

### Generating caches at compile time

Now that we have simplified the method body by implementing the `cached` macro annotation, we also want to get rid
of the explicit `Cache` instance definitions.
In order to achieve that, we need to not only transform a definition, but generate an entirely new
definition as well, namely the cache.
Luckily, macro annotations allow us to expand an annottee in as many ASTs as we want.
This allows us to expand the annotated method in both the transformed method and an additional value definition for
the cache. The `impl` method of the macro annotation is adapted as follows:

```scala
def impl(c: whitebox.Context)(annottees: c.Tree*): c.Tree = {
  import c.universe._

  annottees.head match {
    case q"$mods def $method[..$typeParams](...$params): $returnType = $rhs" =>

      val (paramNames, paramTypes) = params.flatten.map {
        case q"$_ val $name: $tpt = $_" => name -> tpt                                 // (1)
      }.unzip

      val cacheName = TermName(c.freshName(method + "_generatedCache"))                // (2)
      val keyName = TermName(c.freshName("key"))
      val resultName = TermName(c.freshName("result"))

      val newRhs =...
      val expandedMethod =
        q"$mods def $method[..$typeParams](...$params): $returnType = $newRhs"

      val cacheKeyType = tq"(..$paramTypes)"                                           // (3)
      val cache =                                                                      // (4)
        q"""
         private val $cacheName: de.codecentric.Cache[$cacheKeyType, $returnType] =    
           new de.codecentric.MapCache[$cacheKeyType, $returnType]
         """

      q"$expandedMethod; $cache"                                                       // (5)

    case annottee => c.abort(annottee.pos, "Annottee must be a method")
  }
}
```

In (1) we extract the types of the method parameters, which we then use in (3) to define the key type of the cache.
The interpolator `tq"(..$paramTypes)"` is used to build a tuple type of the `paramTypes`. 
The `tq"..."` notation is similar to `q"..."`, which we learned previously, with the difference that the latter is used
to build expression trees, while the former is used for type trees.

Instead of referencing a cache definition from application code, our new `cacheName` in (2) will reference a generated
value definition.
We use a fresh name for the cache, which avoids naming conflicts and also allows us to annotate multiple overloaded methods
that share the same name but with different method signatures.

The cache definition is generated in (4). It uses the unique `cacheName` as name, the tuple of the method parameter types
as key type and the return type as value type. We use the `MapCache` implementation as before.

Finally, in (5) we return both the `expandedMethod` and the `cache` as two individual definitions created from a single
annottee.


With the caches generated from macro transformation as well, our application code reduces to only the methods with
the `@cached` annotation. 
Any trace of actual caching logic is removed and deferred to code generation at compile time.

```scala
@cached
def f(x: Int, y: Int): Int = x * y

@cached
def g(x: Int, s: String): String = x.toString + s
```

### Custom cache implementations from implicit cache factories

So far we have used a simple `MapCache` implementation for the caches. 
In a real-world example, one would rather use a more advanced caching solution, which provides features such as a
size limit and expiration of entries.
For example, we could use a cache implementation based on *Guava*, that would implement our previously defined
`Cache` trait:

```scala
class GuavaCache[K, V] extends Cache[K, V] {
  
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
```

We would like to be able to use different Cache implementations with the `@cached` annotation without having to
touch the macro transformation for each one.
An easy way to achieve that, is by resolving the cache implementation from the implicit scope.
First, we provide a `CacheFactory` that offers an `apply` method producing a `Cache`:

```scala 
trait CacheFactory {
  def apply[K, V](): Cache[K, V]
}
```

An example implementation using the previously defined GuavaCache would look as follows.
It is defined as implicit object in order to allow the generated code to resolve it:

```scala
implicit object GuavaCacheFactory extends CacheFactory {
  override def apply[K, V](): Cache[K, V] = new GuavaCache[K, V]
}
```

In the generated code, we replace the explicit creation of a `MapCache` with a call of Scala's `implicitly` method,
which receives the `CacheFactory` as type parameter.
The `apply` method is called on the resolved `CacheFactory`, yielding a `Cache` of the appropriate key and value type:

```scala
val cache =
  q"""
   private val $cacheName: de.codecentric.Cache[$cacheKeyType, $returnType] = 
     implicitly[de.codecentric.CacheFactory].apply[$cacheKeyType, $returnType]()
  """
```

During implicit resolution, the compiler will look for an implicit definition that fits the required `CacheFactory` type and thereby
find the `GuavaCacheFactory`.

## Transferring the implementation to Scala 3

Macro annotations have only recently been added to Scala 3 in the pre-release version 3.3.0-RC2 of the Dotty compiler.
They differ from Scala 2 macro annotations with regard to when they are expanded during compilation.
While in Scala 2, macro annotations are expanded *before type-checking the AST*, in Scala 3, they are expanded
*after type-checking*.
This decision was made intentionally by the language designers, in order to ensure safety and robustness and
improve IDE support. [[2]](https://www.scala-lang.org/blog/2018/04/30/in-a-nutshell.html)

However, this decision makes the implementation of Scala 3 macro annotations currently more verbose.
In Scala 2, we could mostly write Scala code in quasiquotes and use it as output of our macro implementation.
Meanwhile, in Scala 3, we additionally have to take care of symbols, unique identifiers of definitions,
which especially makes adding new members to a class more complicated than in Scala 2.
Further, it is no longer possible to use implicit resolution to pass a value from application
code into the code generated by the macro.
This is due to the fact that macro expansion now happens after implicits are resolved by the compiler.
An `implicitly` in macro code will not be resolved from an implicit (or `given`) value in application code.

With this in mind, let's have a look at the implementation of the `cached` macro annotation in Scala 3.
The implementation transforms the annotated method and adds a new definition for the cache.
However, it does not make the cache implementation configurable as the last stage of our `cached` annotation in
Scala 2, due to the limitation mentioned earlier. The code for the Scala 3 macro annotation can be found [here](https://github.com/LukasL97/scala-3-macros-example).

```scala
@experimental
class cached extends MacroAnnotation {                                                 // (1)
  override def transform(using q: Quotes)(
    tree: quotes.reflect.Definition
  ): List[quotes.reflect.Definition] = {
    import q.reflect._

    tree match {
      case DefDef(name, params, returnType, Some(rhs)) =>                              // (2)

        val flattenedParams = params.map(_.params).flatten                             // (3)
        val paramTermRefs = flattenedParams.map(
          _.asInstanceOf[ValDef].symbol.termRef)
        val paramTuple = Expr.ofTupleFromSeq(paramTermRefs.map(Ident(_).asExpr))
        
        (paramTuple, rhs.asExpr) match {
          case ('{ $p: paramTupleType }, '{ $r: rhsType }) =>                          // (4)

            val cacheName = Symbol.freshName(name + "Cache")                           // (5)
            val cacheType = TypeRepr.of[Cache[paramTupleType, rhsType]]
            val cacheRhs = '{ new MapCache[paramTupleType, rhsType] }.asTerm
            val cacheSymbol = Symbol.newVal(
              tree.symbol.owner, cacheName, cacheType, Flags.Private, Symbol.noSymbol)
            val cache = ValDef(cacheSymbol, Some(cacheRhs))
            val cacheRef = Ref(cacheSymbol).asExprOf[Cache[paramTupleType, rhsType]]

            def buildNewRhs(using q: Quotes) = {                                       // (6)
              import q.reflect._
              '{
                val key = ${ paramTuple.asExprOf[paramTupleType] }
                $cacheRef.get(key) match {
                  case Some(value) =>
                    value
                  case None =>
                    val result = ${ rhs.asExprOf[rhsType] }
                    $cacheRef.put(key, result)
                    result
                }
              }
            }
            val newRhs = buildNewRhs(using tree.symbol.asQuotes).asTerm
            
            val expandedMethod = DefDef.copy(tree)(
              name, params, returnType, Some(newRhs))                                  // (7)

            List(cache, expandedMethod)
        }
      case _ =>
        report.error("Annottee must be a method")                                      // (8)
        List(tree)
    }
  }
}
```

We start in (1) by implementing the `cached` annotation as subclass of `MacroAnnotation`,
the base trait for macro annotations in Scala 3.
We implement the `transform` method, which transforms the annotated definition in a list of possibly
multiple definitions.
The `transform` method has an additional implicit parameter `q` of type `Quotes`, which provides programmatic
access to the reflection API of Scala 3, similar to the `Context` in Scala 2.

In (2), the input AST is matched against a `DefDef` which is the definition of a method.
If the annottee is not a method, we report a compilation error in (8).

We first process the parameters of the method in (3), mapping them to an expression that represents
a tuple of the input parameters.
In Scala 2, we could just do this using a dedicated quasiquotes notation.
Now, we have to extract the symbols from the parameter definitions and build references to their identifiers.
The method `Expr.ofTupleFromSeq` allows us to build a tuple expression from a list of expressions.

In (4), we pattern match the parameter tuple expression and the method's right-hand side against *quotes*.
Quotes are conceptually similar to quasiquotes in Scala 2.
They are written as `'{ ... }` and can contain any typed Scala code.
In this case, we use them to extract the types of the parameter tuple and the method body, which we will use
as key and value type parameters of the cache.

We then build the cache definition in (5), starting with a fresh name.
We construct a type representation of the `Cache` type, with the key and value type parameters we extracted in (4).
The right-hand side of the cache definition is written as a quote in which we create a `MapCache`.
Then, we create the `cacheSymbol` as a symbol representing a `val`.
As parent we use the owner of the expanded method, i.e., the class in which the method is defined.
We then create the `cache` as `ValDef`, the definition of a `val`, using the symbol and right-hand side we
just created.
The `cache` will later be returned alongside the transformed input method.
We also need an additional reference to the `cacheSymbol`, in order to access the new cache definition inside
the transformed body of the annotated method.
In Scala 2, it was possible to just use the term name in a quasiquote.
Scala 3 however does require a `Ref` to the symbol explicitly.

The transformation of the method body happens in (6).
We use a helper method `buildNewRhs` to which we pass the symbol of the original method `asQuotes`.
This is necessary in order to ensure that any definitions created in the quote that describes our new
right-hand side are owned by the method.
Otherwise, the definitions inside the new right-hand side would be owned by the enclosing class and the
code would not compile.
The quote in `buildNewRhs` contains the transformed method body, which is analogous to the Scala 2 implementation.
The reference to the cache is inserted into the quote as `$cacheRef`, which is a *splice*.
Splices allow us to insert expressions into quoted code blocks, evaluating them before the surrounding code.
Similarly, we insert the `paramTuple` and the original `rhs` into the quote.
For the code to type-check, we have to cast them explicitly to expressions of the expected types.
It has to be noted, that we did not use fresh names for the `key` and `result` definitions in the new
method body.
This would of course still be possible and require a similar approach as with the `cacheRef`, creating
a new symbol for each and then references to the symbols, which can then be used inside the quoted code block.
For simplicity, this is omitted here, and we confine ourselves to using the static names `key` and `result`,
which could potentially have naming conflicts to method parameters.

Finally, in (7) we create the `expandedMethod` as a copy of the input method, replacing the original
right-hand side with the `newRhs`.
Then, we return it alongside the `cache`.

## Conclusion

Using the caching example, we have seen how macro annotations can be used in both Scala 2 and Scala 3 to generate code
at compile time.
The `cached` annotation can be reused and is independent of the signature of the method it is applied to.
Also, we bypass the overhead of defining a cache for every method whose return values we want to cache,
by generating it as new definition for every annotated method.

While macro annotations provide an interesting way to use metaprogramming in Scala, they also are somewhat clumsy to use
in practice, especially with regard to IDE support, which is not always working properly.
Taking IntelliJ IDEA using the Scala plugin as an example, the IDE is often unable to infer the type of calls to the
reflection API properly.
This is still an issue in Scala 3, at least at the moment.
However, with macro annotations just having been added as experimental feature to Scala 3, there is some hope that
this might change in the future.
Quasiquotes in Scala 2 are also somewhat difficult to work with, as they are essentially just strings containing
Scala code, which are not type-checked in any form by the IDE plugin.
In this regard, quotes in Scala 3 are an improvement, as they are actually interpreted as Scala code by the IDE.

## References

[1] https://docs.scala-lang.org/overviews/macros/overview.html

[2] https://www.scala-lang.org/blog/2018/04/30/in-a-nutshell.html
