# Compile-time Code Generation with Scala Macros

TODO: introduction / motivation

## Types of Macros

### Def Macros

### Macro Annotations

## Caching Method Return Values with Macro Annotations

As a simple example for macros in Scala we will implement a macro annotation for caching return values of arbitrary methods.
The cache will store key-value pairs of the input parameters of the method and its return value and allows the method to
reuse results of previous inputs. To this end we define a `Cache` trait with a simple `MapCache` implementation:

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

Before introducing the macro annotation, we first want to have a look on how an implementation without macros could look like.
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

The generic `cached` method already provides some simplification for implementing the methods whose results we want to cache.
However, we still have to define a cache for each method and pass it along with the input parameters to the `cached` method.
This will no longer be necessary when we replace the `cached` method with a macro annotation.

### Generating the Caching Logic

We introduce a `cached` macro annotation which can be used to annotate a method of arbitrary signature, providing all
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
Multiple annottees can occur, for instance, if a class with a companion object is annotated.
In this case, both the class and its companion object are passed to the `macroTransform` method and can be transformed
in its implementation.

The context provides access to Scala's reflection API, including features such as quasiquotes and term names, that we
will make use of in the transformation code. We can import those directly as `import c.universe._`.

Now, let's have a look at the implementation of the macro transformation. 
This may seem overwhelming at first, but we will give a step-by-step explanation below.

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
As seen in this example, quasiquotes can also be used for pattern matching, allowing to extract parts of the AST.
Conceptually, this is similar to normal string interpolation with `s"..."`, which allows inserting or extracting parts
of the string.
Note however, that in quasiquotes, we extract parts of the AST instead of strings.
For instance, `$mods` are the modifiers of the annotated method (e.g. `private`) and `rhs` is the expression on the
right-hand side of the annottee. 
Also note how the parameters and type parameters of the method are extracted.
The `..$typeParams` notation indicates a list of trees (in this case type trees).
Meanwhile, the `...$params` notation indicates a nested list of lists of trees, the possibly multiple parameter lists
of the annotated method.

The pattern matching ensures, that the annottee is indeed a method.
Otherwise, we jump to the default case (6), where we abort compilation with a respective error message.

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

Now, we can annotate arbitrary methods as `@cached`. 
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

### Generating Caches at Compile Time

Now that we have simplified the method body by implementing the `cached` macro annotation, we also want to get rid
of the explicit cache definitions. 
In order to achieve, we need to not only transform a definition, as we already do, but generate an entirely new
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
      val cache =
        q"""
         private val $cacheName: de.codecentric.Cache[$cacheKeyType, $returnType] =    // (4)
           new de.codecentric.MapCache[$cacheKeyType, $returnType]
         """

      q"$expandedMethod; $cache"                                                       // (5)

    case annottee => c.abort(annottee.pos, "Annottee must be a method")
  }
}
```

In (1) we extract the types of the method parameters, which we then use in (3) to define the key type of the cache.
The string interpolation `tq(..$paramTypes)` is used to build a tuple type of the `paramTypes`. 
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

### Custom Cache Implementations from Implicit Cache Factories

So far we have used a simple `MapCache` implementation for the caches. 
In a real-world example, one would rather use a more advanced caching solution, which provides features such as a
size limit and expiration.
For example, we could use a cache implementation based on *Guava*, that we would implement our previously defined
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

Now, we would like to be able to use different Cache implementations with the `@cached` annotation without having to
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
which receives the cache type including key and value type as type parameter:

```scala
val cache =
  q"""
   private val $cacheName: de.codecentric.Cache[$cacheKeyType, $returnType] = 
     implicitly[de.codecentric.CacheFactory].apply[$cacheKeyType, $returnType]()
  """
```

During implicit resolution, the compiler will look for an implicit definition that fits the required type and thereby
find the `guavaCacheFactory`.

## Conclusion