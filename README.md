# The Problem

Currently `Kotlin` doesn't support `GADT`, more precisely, dependent
pattern matching, which seems to be natural for the language and exists
in a number of languages like `Scala`, `OCaml`, `Haskell`, and so on.
The simplest example which doesn't type-check is

```Kotlin
sealed class Expr<out T>
data class ExprIntLit(val i: Int) : Expr<Int>

fun <T> eval(e: Expr<T>): T = when (e) {
    is ExprIntLit -> e.i // Type mismatch. Required: T Found: Int
}
```

or

```Kotlin
interface Expr2<out T>
class Expr2IntLit(val i : Int) : Expr2<Int>

fun <T> eval(e: Expr2<T>): T = when (e) {
    is Expr2IntLit -> e.i // Type mismatch. Required: T Found: Int
    else -> TODO()
}
```

I.e. type-checker is not able to locally cast `e.i` to the required type
although it has all the necessary information. The paper presents a
proposal how the current type-checking can be modified in order to
support dependent pattern-matching.

# Motivation

## Scope

In general GADT inference is associated with the pattern-matching when
we match a value of the sum type on one of their constructors and able
to specialize the type parameters of the sum type based on their
instances in the specific constructor. It is how the GADT inference
working in the functional languages with their system of subtyping.
While it is not a case for languages with OOP-style subtyping as it is
shown in \[ref to C# paper\] and Scala 3 implementation and this will be
described in algorithm section. The fact is that we are able to run any
inference in the moment we are identifying that there is a value in the
program that have two types (Sum type and type of the specific
constructor in case of functional languages and two arbitrary types in
case of Kotlin). Luckily Kotlin already have some kind of flow typing
that extracts operational information that some value must have several
type in specific branch. As such analysis is not limited to the when
expression, we are able to successfully infer types not in such case:

```Kotlin
sealed class Expr<out T>
data class ExprIntLit(val i: Int) : Expr<Int>

fun <T> eval(e: Expr<T>): T = when (e) {
    is ExprIntLit -> e.i
}
```

but also in cases like

```Kotlin
sealed class Expr<out T>
data class ExprIntLit(val i: Int) : Expr<Int>

fun <T> eval(e: Expr<T>): T {
    if (e is ExprIntLit) {
        return e.i
    }
    TODO()
}
```

```Kotlin
sealed class Expr<out T>
data class ExprIntLit(val i: Int) : Expr<Int>

fun <T> eval(e: Expr<T>): T {
    if (e !is ExprIntLit) {
        TODO()
    }
    return e.i
}
```

and even

```Kotlin
sealed class Expr<out T>
data class ExprIntLit(val i: Int) : Expr<Int>

fun <T> eval(e1: Expr<T>, e2: Expr<Int>): T {
    if (e1 === e2) {
        return e.i
    }
    TODO()
}
```

As in all this cases we have an information that there is a value that
is subtype of both, `ExprIntLit` and `Expr<T>`.

The difference with the current implementation is that it collects such
statements only for variables (as temporal values are not eligible for
smart casts), but temporal values are outfiltered on the last stage so
it is not hard to collect such statement for any values. And it will
allow us to support even such cases:

```Kotlin
sealed class Expr<out T>
data class ExprIntLit(val i: Int) : Expr<Int>

fun <T> eval(e: Expr<T>): T {
    if (transform(e) is ExprIntLit) {
        return e.i
    }
    TODO()
}
```

## Generic use-cases

[Source and more
examples](https://chrilves.github.io/posts/gadts_by_use_cases/)

### Runtime subtyping evidence {#runtime-subtyping-evidence .unnumbered}

One easy, but very useful, benefit of GADTs is expressing relations
about types like \"$A <: B$\" or \"$A = B$\":

```Kotlin
sealed interface EqT<A, B>{
  class Evidence<X>() : EqT<X, X>
}

sealed interface SubT<A, B>{
  class Evidence<A, B : A>() : EqT<A, B>
}
```

It may be used like this:

```Kotlin
fun <A, B> coerce(subT: SubT<B, A>, a: A): B =
  when (subT) {
    is SubT.Evidence<*> => a // Inferred: B :> A
  }
```

While in this example, we were able to express this in the constraints
for generic parameter, it is still may be useful for example in case we
have a complex collection which can be slightly optimized based on any
property of the stored type (for example if they are comparable). We may
do not want to write another implementation as it is not significant but
we would like to optimize it while it's possible. We may take such
property as a boolean but it will lead to the error-prune type casts.
Instead of this we may use:

```Kotlin
sealed interface Comparability<A>
class Comparable<A : Comparable<A>>() : Comparability<A>
class NotComparable<A>() : Comparability<A>

...

class ComplexCollection<V>(val comparability: Comparability<V>) {
  private val values: List<V>

...

  fun doAlgorithm() {
    when (comparability) {
      is Comparable<*> -> optimizedAlgorithm(values)
      is NotComparable<*> -> defaultAlgorithm(values)
    }
  }
  
...

fun <V : Comparable<V>> optimizedAlgorithm(values: List<V>)
fun <V> defaultAlgorithm(values: List<V>)
```

### More type-safe extensions {#more-type-safe-extensions .unnumbered}

Let's imagine library with such architecture:

```Kotlin
sealed interface Chart<A> {
    fun draw(chartData: A)
}
class PieChart : Chart<PieData>
class XYChart : Chart<XYData>
```

If we would like to write an extension that will draw chart in another
way, than it may looks like this:

```Kotlin
fun <A> Chart<A>.myDraw(chartData: A): Unit =
  when (chart) {
    is PieChart -> {
      val pieData = chartData as PieData
      ... // modify
      draw(pieData)
    }
    else -> draw(chartData)
  }
```

Programmer have to explicitly cast data to PieData as he is sure that it
is always successful. In this case it is true and could be inferenced
gadt inference. Then code could became more type-safe and less verbose:

```Kotlin
fun <A> Chart<A>.myDraw(data: A): Unit =
  when (chart) {
    is PieChart -> {
      // chartData is PieData in this branch
      ... // modify
      draw(chartData)
    }
    else -> draw(chartData)
  }
```

Another real-world example is the following:

```Kotlin
@Suppress("UNCHECKED_CAST")
internal operator fun <T : CommonCompilerArguments> get(compilerArgumentsClass: Class<T>): T = when (compilerArgumentsClass) {
    K2MetadataCompilerArguments::class.java -> k2MetadataCompilerArguments as T
    K2JVMCompilerArguments::class.java -> k2JvmCompilerArguments as T
    K2JSCompilerArguments::class.java -> k2JsCompilerArguments as T
    else -> commonCompilerArguments as T
}
```

With GADT inference we would be able to remove type casts at least in
the first three branches

## Real-world use-cases

todo: bootstrap compiler and count redundant casts

todo: bigcode analyzer for Scala

fs2 in Scala lacks of GADT and simulates it through abstract methods:
[docs](https://github.com/typelevel/fs2/blob/fbd0f25238f0321474816375f1992ecc10e1cc3e/docs/implementation-notes.markdown)

https://github.com/higherkindness/mu-scala

https://github.com/AdrielC/free-arrow

https://www.informatik.uni-marburg.de/ kos/papers/gpce10.pdf

https://github.com/milessabin/shapeless

https://github.com/owlbarn/owl

For Scala:

Akka: Although not specifically a GADT-focused library, Akka (a toolkit
for building highly concurrent, distributed, and resilient
message-driven applications) has utilized advanced type system features
in Scala, including GADTs, for internal implementations to enforce
certain invariants at compile-time.

Slick: Slick (Scala Language-Integrated Connection Kit) is Functional
Relational Mapping (FRM) library for Scala that makes it easy to work
with stored data in a type-safe manner. It uses GADTs to represent
abstract syntax trees for database queries in a type-safe manner.

[bugs](https://github.com/lampepfl/dotty/issues?q=label%3Aitype%3Abug+label%3Aarea%3Agadt)

# Sources

1.  [Presentation of the implementation in Scala 3 from the typelevel
    summit](https://www.youtube.com/watch?v=VV9lPg3fNl8)

2.  [Outline of the implementation in Scala
    3](https://dl.acm.org/doi/pdf/10.1145/3563342) (Section 6.2)

3.  [First formalization for
    C#](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/gadtoop.pdf)
    (Section 5.1)

4.  [Dotty PR 1](https://github.com/lampepfl/dotty/pull/5736), [Dotty PR
    2](https://github.com/lampepfl/dotty/pull/6398)

# Compared to Scala implementation

To implement it in Kotlin may be quite easier than in Scala as for each
supertype of the type, Kotlin's generics may have only one value. For
example, following code:

```Scala
trait A[+T]
trait B[T] extends A[T]
class C extends B[Object] with A[Int]
```

is valid in Scala while the same code in Kotlin:

```Kotlin
interface A<in T>
interface B<T> : A<T>
class C : B<Object>, A<Int>
```

fails to compile with error: "Type parameter `T`{.Kotlin} of
'`A`{.Kotlin}' has inconsistent values: `Object, Int`{.Kotlin}".

# Algorithm

The algorithm arises when we would like to intersect types (locally). It
aims to specialize all of the types in the intersection based on the
information that there is the value with the intersected type. This
algorithm could be merged with the smart-casting as it infers the
required information. Algorithm consist of two parts, generation of
subtyping and equalities constraints and their resolution.

## Generation of the constraints

There is the type of variable and runtime value. *Type* consists of
classes and their type parameters. We can project type on class and get
the type parameters of that class for that type. Let $S$ be a type of
scrutenee, \"$T$ is a "type of pattern".

1.  If $S$ is an intersection type then run the following algorithm for
    each type in intersection.

2.  Intersect types $S$ and $T$

3.  Find all common classes (super-types) of types $S$ and $T$, denote
    as $T_n$

4.  For each class $T_n$ do:

    1.  Generate type parameters:

        1.  Project type $S$ on class $T_n$ and receive type parameters
            $P_{S}^{T_n}$ storing for each parameter information if it
            depends on co- and contra- variant positions in the lowest
            class of $S$.

        2.  Project type $T$ on class $T_n$ and receive type parameters
            $P_{T}^{T_n}$ storing for each parameter information if it
            depends on co- and contra- variant positions in the lowest
            class of $T$.

    2.  Generate constraints: for each parameter position

        1.  If position is invariant then $p_T^{T_n} = p_S^{T_n}$ for
            that position.

        2.  If it is not invariant, then

            1.  If both of the parameters do not depends on the co- and
                contra- variant positions then both of them are equal to
                the real parameters and equal to each other.
                Consequently, we can add the constraint
                $p_T^{T_n} = p_S^{T_n}$ for that position.

            2.  If any of the parameters do not depends on the co- and
                contra- variant positions then that parameter is equal
                to the real parameter and have to be a subtype (for
                covariant position) or supertype (for contravariant
                position) of the equal type. Consequently, we can add
                the constraint $p_?^{T_n} :> p_?^{T_n}$ for that
                position.

            3.  If both of them depends on the co- and contra- variant
                positions then we only can establish that both of them
                are subtypes (for covariant position) or supertypes (for
                contravariant position) of the real type and we could
                not get any information in this case.

## Constraints resolution

I guess it's obvious. The only complicated case that comes in mind is
the following:

### Intersection types

If we would like to satisfy a constraint `A :> B & C` we result in a
disjoint constraints `A :> B | A :> C` which is not easy to solve. As I
got from [this moment of
presentation](https://youtu.be/VV9lPg3fNl8?t=1391), if they met the
situation that leads to disjoint constraints, they just do not add such
constraints. From the next slide they said that if all-except-one of the
disjoint constraints are unsatisfied, than we could process such a
constraint.

# Relation to smart casts

I guess that we could use the same technique to infer the most precise
type for smart cast after the intersection. We just should not remove
constraints related to the real types and solve them with the other
constraints (or after them). Also we could use the firs type of the
constraints' group for the inference of the most precise bound for the
smart cast. For example, in such case:

```Kotlin
interface A<in T, in V>
interface A1<in V> : A<Int, V>
interface A2<in T> : A<T, Int>

fun f(v: A1<*>) {
    val vv : A2<Int> = when (v) {
        is A2<*> -> v
        else -> throw Exception()
    }
}
```

we could infer the type `{A1<Int> & A2<Int>}` instead of the current
`{A1<*> & A2<*>}`. For example it'll allow us to cast that type to the
`A<Int, Int>`

# Questions for implementation

1.  Is the current limitations for the bounds allows us to express all
    of the inferred bounds? What is the current limitations in the
    compiler?

2.  How to add bounds for the variables in the expression? Is it could
    be in the same way as for smart casts?

3.  In first example (below) we achieve a constraints $B :> * :> A$.
    Such cases should be considered during the implementation.

# Examples

1.  In such code:

    ```Kotlin
    interface Func<in A, out B>

    class Identity<X> : Func<X, X>

    fun <A, B> foo(func: Func<A, B>): B {
        when (func) {
            is Identity<*> -> {
                val b: B = (TODO() as A) // valid assignment
            }
            else -> TODO()
        }
    }
    ```

    We will get a constraints $B :> * :> A$ and could establish that $A$
    is a subtype of $B$. This could not be inferred in Scala as such
    class:

    ```Scala
    class FalseIdentity extends Identity[Any] with Func[Any, Nothing]
    ```

    would be valid for them but not for Kotlin.

2.  For code showing the unsoundness of the GADTs in Scala 2:

    ```Kotlin
    open class C<out T>
    data class D<S>(var s: S) : C<S>()

    fun main() {
      val x = D("")
      val y: C<Any> = x

      when (y) {
        is D<*> -> {
          // Should not typecheck! 
          // But it does in Scala2, fixed in Scala3
          y.s = Integer(1) // Scala2: y is D<Any> 
        }
      }
      val z: String = x.s as String
    // Scala2: ClassCastException: Integer cannot be cast to String
    }
    ```

    We will result a constraints $S_{real} <: Any$ which is not enough
    to infer $S_{real}$ due to absence of variance in $D$. (We could
    generate existential variable in that place and add a constraint for
    it, but I guess that it would not worth it, except if Kotlin already
    supports an existential variables in the compiler)

3.  Example with disjunction constraints. (too hard, todo)
