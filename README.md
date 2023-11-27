# Prototype implementation reference

https://github.com/e2e4b6b7/kotlin/pull/2

# The Problem and Motivation

`Kotlin` currently supports algebraic data types, or *ADT*, via sealed classes and interfaces.
ADTs allows one to form a type by combining other types.
The beauty comes when ADTs are equipped with pattern-matching (`when` expressions) -
matching a value against a pattern.

In functional programming (languages like `Scala`, `OCaml`, `Haskell`, and so on) the concept of generalized algebraic data types, or *GADT* (aka. guarded recursive datatype), is widely used.
It is a generalization of parametric algebraic data types by permitting value constructors to return specific, rather than parametric, type-instantiations of their own datatype.
GADTs enables the encapsulation of additional type information (invariants) in ADTs, along with the ability of utilizing the encapsulated type information when performing pattern matching.

One of the classical well-known GADT use-cases is ensuring type safety when defining DSLs.
For example (in `Scala`), an arithmetic expression can be type-safe by construction disallowing one to even construct an expression that uses binary operator on non-numerical values and tuples:
```Scala 
enum Expr[A]:
  case LitInt(i: Int) extends Expr[Int]
  case Add(e1: Expr[Int], e2: Expr[Int]) extends Expr[Int]
  case Tuple[X, Y](x: Expr[X], y: Expr[Y]) extends Expr[(X, Y)]
```
In the example constructor `LitInt` asserts that the data being created is an `Expr[Int]`, 
not just some generalized `Expt[T]`, while binary addition constructor `Add` asserts that its sub-expressions are numbers (are of type `Expr[Int]`).
Thus, in this case our `invariants` are: any integer literal is actually an integer, while any binary addition has both sub-expressions of an integer type.
The main advantage is that this information is encapsulated in the type itself.
Note, there is no way to construct an ill-formed expression (for example, addition to tuples).

As ADTs comes with pattern-matching, GADTs comes with generalized pattern-matching that utilizes the information encoded/encapsulated in GADTs ensuring source code type safety.
In other words, GADTs themselves represent *types correct by construction* while generalized pattern matching *guarantee absence of type errors during evaluation*.

For example (in `Scala`), the following function that evaluates arithmetic expressions is well-typed.
In the `LitInt` case: GADT constraint `Int = T` allows the branch to return `Int` instead of `T`.
Moreover, in the `Add` case: we can safely use binary addition.
That is, we locally use the information encapsulated in GADTs in each branch of pattern-matching,
ensuring the branch is well-typed with respect to this information and "forgetting"
the information going beyond the branch.
```Scala
def eval[T](e: Expr[T]): T = e match
  case LitInt(i) => i
  case Add(e1, e2) => eval(e1) + eval(e2)
  case Tuple(x, y) => (eval(x), eval(y))
```

GADTs have a number of applications, including DSls definition, strongly-typed evaluators,
generic pretty-printing, generic traversals and queries, databases, and typed parsing.

Unfortunately, *`Kotlin` has no support for dependent pattern matching while allowing one to actually define a `GADT`*.
This leads to some kind of inconsistency in the language design and unlikely compiler behavior.
For example, the following code does not type check, i.e., type-checker is not able to locally cast `e.i`,
even though a type checker has the all necessary information.


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

***The paper presents a proposal how the current type-checker can be modified in order to cover this `gap` in the language design by adding support for dependent pattern-matching.***

***Moreover, adding the mechanism to support for generalized pattern matching in type-checker also improves smart-casts behaviour and allows one to get rid of a number of unsafe casts in source code.*** (See next section for details)


## The Problem Scope (or Accompanying Benefits)

In general, GADT inference is associated with the pattern-matching when we match a value of the sum type on one of their constructors and able to specialize the type parameters of the sum type based on their instances in the specific constructor.
It is how the GADT inference works in the functional languages with their system of subtyping.
While it is not the case for languages with OOP-style subtyping as it is shown in \[ref to C# paper\] and Scala 3 implementation (see an **Bounds inference algorithm** section below for details).
In fact, we are able to run GADT inference in the moment we are identifying that there is a value in the program that has two types, i.e., sum type and type of the specific constructor in case of functional languages, and two arbitrary types in case of Kotlin.
Luckily, in Kotlin, in order to support smart casts, there exists a kind of flow typing that extracts operational information about different types of a value in a specific branch.
Since this information is not limited to the `when`-expressions only, we are able to successfully infer types not only in case of `when`-expressions but also in branches of other control-flow operators like conditional branching and so on.
For example, all four examples below are well-typed as in all these cases we have an information that there is a value that is subtype of both `ExprIntLit` and `Expr<T>`.

```Kotlin
sealed class Expr<out T>
data class ExprIntLit(val i: Int) : Expr<Int>

fun <T> eval(e: Expr<T>): T = when (e) {
    is ExprIntLit -> e.i
}
```

```Kotlin
fun <T> eval(e: Expr<T>): T {
    if (e is ExprIntLit) {
        return e.i
    }
    TODO()
}
```

```Kotlin
fun <T> eval(e: Expr<T>): T {
    if (e !is ExprIntLit) {
        TODO()
    }
    return e.i
}
```

```Kotlin
fun <T> eval(e1: Expr<T>, e2: Expr<Int>): T {
    if (e1 === e2) {
        return e.i
    }
    TODO()
}
```


The current implementation that collects such statements only for variables (as temporal values are not eligible for smart casts) making last three examples ill-typed.
But since temporal values are outfiltered on the very last stage, we can easily collect these statements for any values.
As a consequence, even the following case becomes well-typed.

```Kotlin
fun <T> transform(e: Expr<T>): Expr<T> = TODO()

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

### Runtime subtyping evidence

One easy, but beneficial, benefit of GADTs is expressing relations
about types like \" $A <: B$\" or \" $A = B$\":

```Kotlin
sealed interface EqT<A, B>{
  class Evidence<X> : EqT<X, X>
}

sealed interface SubT<A, B>{
  class Evidence<A, B : A> : EqT<A, B>
}
```

It may be used like this:

```Kotlin
fun <A, B> coerce(subT: SubT<B, A>, a: A): B =
  when (subT) {
    is SubT.Evidence<*> -> a // Inferred: B :> A
  }
```

While in this example, we were able to express this in the constraints for generic parameter. 
Nevertheless, it may be useful, for example, in case we
have a complex collection which can be slightly optimized based on any
property of the stored type (for example, if they are comparable). 

Currently available solutions:

* Write another implementation for each property value. 
  Require much additional code, abstract classes and may lead to code duplication.
* Get comparability property as a boolean or enum parameter.
  In this case, 
  we have to write explicit error-prone casts in each place where we require comparability.

With gadt inference, we can express it more conveniently:

```Kotlin
sealed interface Comparability<A> {
    class Comparable<A : kotlin.Comparable<A>> : Comparability<A>
    class NotComparable<A> : Comparability<A>
}

...

class ComplexCollection<V>(val comparability: Comparability<V>) {
  private val values: List<V>

...

  fun doAlgorithm() {
    when (comparability) {
      is Comparability.Comparable<*> -> optimizedAlgorithm(values)
      is Comparability.NotComparable<*> -> defaultAlgorithm(values)
    }
  }
  
...

fun <V : Comparable<V>> optimizedAlgorithm(values: List<V>)
fun <V> defaultAlgorithm(values: List<V>)
```

### More type-safe extensions

Let's imagine a library with such an architecture:

```Kotlin
sealed interface Chart<A> {
    fun draw(chartData: A)
}
class PieChart : Chart<PieData>
class XYChart : Chart<XYData>
```

If we would like to write an extension that will draw chart in another
way, then it may look like this:

```Kotlin
fun <A> Chart<A>.myDraw(chartData: A): Unit =
  when (this) {
    is PieChart -> {
      val pieData = chartData as PieData
      ... // modify
      draw(pieData)
    }
    else -> draw(chartData)
  }
```

Programmer have to explicitly cast data to PieData as he is sure that it
is always successful. In this case, it is true and could be inferenced
gadt inference. Then code could become more type-safe and less verbose:

```Kotlin
fun <A> Chart<A>.myDraw(chartData: A): Unit =
  when (this) {
    is PieChart -> {
      // chartData is PieData in this branch
      ... // modify
      draw(chartData)
    }
    else -> draw(chartData)
  }
```

## Real-world examples

1.  [GitHub link](https://github.com/JetBrains/kotlin/blob/242c1cf5f0814fbe9df02b4b85a63298b30b4b67/core/reflection.jvm/src/kotlin/reflect/jvm/internal/calls/ValueClassAwareCaller.kt#L45)
2.  [GitHub link](https://github.com/JetBrains/kotlin/blob/242c1cf5f0814fbe9df02b4b85a63298b30b4b67/compiler/resolution/src/org/jetbrains/kotlin/resolve/calls/KotlinCallResolver.kt#L165)
3.  [GitHub link](https://github.com/JetBrains/kotlin/blob/242c1cf5f0814fbe9df02b4b85a63298b30b4b67/compiler/fir/providers/src/org/jetbrains/kotlin/fir/types/TypeUtils.kt#L211-L21)
4.  [GitHub link](https://github.com/JetBrains/kotlin/blob/242c1cf5f0814fbe9df02b4b85a63298b30b4b67/jps/jps-plugin/src/org/jetbrains/kotlin/jps/model/ProjectSettings.kt#L72-L75)

## Real-world use-cases

There are several papers that discuss the use-cases of GADTs[links].
The main mentioned advantages are:

* Well-typed LR Parser. 
  GADT allows eliminating some runtime checks which improve performance over non-GADTs implementations.
* Type-safe AVL Tree.
  GADT with existential types allows implementing AVL Tree with type-level control of a balance factor.
* Typed Printf/Scanf Formats.
  There is a proposal to use GADTs instead of strings to represent printf/scanf formats in OCaml, 
  which not only improves performance but also fixes potential bugs and stabilizes the code.
* Event Processing Optimizations.
  GADTs enabled a number of optimizations in the area of event processing, 
  which led to performance gains in applications focused on this aspect.
* Optimization of Combinator Libraries.
  
To find existing libraries that use GADTs, 
especially in Scala, whose type system is the most similar to the Kotlin's of languages with GADTs,
requires significant work to analyze the codebase of multiple libraries.
The only one that lies on the surface is fs2, 
which has a [standalone document](https://github.com/typelevel/fs2/blob/fbd0f25238f0321474816375f1992ecc10e1cc3e/docs/implementation-notes.markdown) 
describing how they simulate GADTs in Scala 2.
This document was written before GADTs were well-supported in Scala 3.

### Sources

https://github.com/higherkindness/mu-scala

https://github.com/AdrielC/free-arrow

https://github.com/milessabin/shapeless

https://github.com/owlbarn/owl

https://www.informatik.uni-marburg.de/ kos/papers/gpce10.pdf

http://www.cs.nott.ac.uk/~psznhn/Publications/icfp2005.pdf

http://gallium.inria.fr/~fpottier/publis/fpottier-regis-gianas-typed-lr.pdf

http://pauillac.inria.fr/~fpottier/slides/slides-popl04.pdf

https://www.cs.tufts.edu/~nr/cs257/archive/tim-sheard/lang-of-future.pdf

https://infoscience.epfl.ch/record/98468/files/MatchingObjectsWithPatterns-TR.pdf

# Bounds inference algorithm

This algorithm is aimed to infer bounds for type parameters based on the operational information about the types.

Input of the algorithm is a set of type intersections
that is known to have a value in the specific node of the control-flow graph.
Output of the algorithm is a set of bounds for type parameters used in types in the intersections
(or more generally, reachable in this node of the control-flow graph).

This algorithm could be merged with the smart-casting
as it also infers the bound for the real type as well as for type parameters.
The algorithm consists of two parts, generation of subtyping and equality constraints and their resolution.

## Generation of constraints

There is the type of variable and runtime value. *Type* consists of
classifiers and their type parameters. We can project type on classifier and get
the type parameters of that classifier for that type. Let $S$ be a type of
scrutenee, \" $T$ is a "type of pattern".

1.  If $S$ is an intersection type, then run the following algorithm for
    each type in intersection.

2.  Intersect types $S$ and $T$

3.  Find all least common classifiers (super-types) of types $S$ and $T$, denoted 
    as $T_n$

4.  For each classifier $T_n$ do:

    1.  Generate type parameters:

        1.  Project type $S$ on classifier $T_n$ and receive type parameters
            $P_{S}^{T_n}$ storing for each parameter information if it
            depends on co- and contra- variant positions in the lowest
            classifier of $S$.

        2.  Project type $T$ on classifier $T_n$ and receive type parameters
            $P_{T}^{T_n}$ storing for each parameter information if it
            depends on co- and contra- variant positions in the lowest
            classifier of $T$.

    2.  Generate constraints: for each parameter position

        1.  If position is invariant, then $p_T^{T_n} = p_S^{T_n}$ for
            that position.

        2.  If it is not invariant, then

            1.  If both of the parameters do not depend on the co- and
                contra- variant positions then both of them are equal to
                the real parameters and equal to each other.
                Consequently, we can add the constraint
                $p_T^{T_n} = p_S^{T_n}$ for that position.

            2.  If any of the parameters do not depend on the parameters in
                co- and contra- variant positions of the original type,
                then that parameter is equal
                to the real parameter and have to be a subtype (for
                covariant position) or supertype (for contravariant
                position) of the equal type. Consequently, we can add
                the constraint $p_?^{T_n} :> p_?^{T_n}$ for that
                position.

            3.  If both of them depend on the co- and contra- variant
                positions, then we only can establish that both of them
                are subtypes (for covariant position) or supertypes (for
                contravariant position) of the real type, and we could
                not get any information in this case.

### Compared to Scala

The algorithm is quite different from the Scala's algorithm and may infer bounds in more cases.
The main difference arises from the following paragraph of the Kotlin's specification:

> the transitive closure S∗(T) of the set of type supertypes S(T : \(S_1\), . . . , \(S_m\)) = {\(S_1\), . . . , \(S_m\)} ∪ S(\(S_1\)) ∪ . . . ∪ S(\(S_m\))
> is consistent, i.e., does not contain two parameterized types with different type arguments.

For instance, the following code:

```Scala
trait Func[-A, +B]
trait Identity[X] extends Func[X, X]
trait FalseIdentity extends Identity[Int], Func[Any, Int]
```

is valid in Scala while the same code in Kotlin:

```Kotlin
interface Func<in A, out B>
interface Identity<X> : Func<X, X>
interface FalseIdentity : Identity<Int>, Func<Any, Int>
```

fails to compile with error:
`Type parameter B of 'Func' has inconsistent values: Int, Any`.

As a result for the code like this:

```Kotlin
fun <A, B> foo(func: Func<A, B>) = when (func) {
    is Identity<*> -> {
        val b: B = TODO() as A
    }
    else -> TODO()
}
```

We are able to infer relation $A <: B$ in Kotlin, 
while this is not a case for Scala as we may call this function with `FalseIdentity` as an argument, 
consequently, A would be `Any` and B would be `Int` which does not satisfy $A <: B$.

### Special cases

#### Star projections

The constraint generation is based on the transitive relation of both types to the real (runtime) type of the value.
The only case where it would not work as expected is the star projections.
The issue is that they all are equal to each other,
and we are not able to distinguish them to make correct transitive relations.
For example, in this case:

```Kotlin
sealed interface EqT<A, B>{
  class Evidence<X> : EqT<X, X>
}

fun <A, B> coerce(eqT: EqT<B, A>, a: A): B =
  when (eqT) {
    is EqT.Evidence<*> -> a
  }
```

We will result a constraints $B = \*$, $\* = A$
and could not establish that $A = B$ as we do not even know if these stars are the same.
So we have to replace all star projections with temporal type variables (standing for the real type)
to correctly follow the transitive relations on them.

#### Flexible types

For flexible types, we have to run the algorithm on their upper bound as it is the type which is guaranteed to be a supertype of the real type.

### Examples

#### Constant (effectively invariant) parameter

Let's elaborate this part of the algorithm:

> If any of the parameters do not depend on the co- and contra- variant positions, then that parameter is equal to the real parameter

If we would like to generate constraints from information that 
types `List<T>` and `List<Serializable>` have a common value,
we would not be able to get any information about bounds of T.
To demonstrate, we could consider such a code:

```Kotlin
fun <T> foo(list: List<T>, serializableList: List<Serializable>, value: T) {
    if (list === serializableList) {
        // Based on the condition, we know that 
        // types `List<T>` and `List<Serializable>` have a common value here
        // But the following cast is not valid (see bar function):
        val serializableValue: Serializable = value
    }
}

fun bar() {
    val list = listOf(1)
    val comparableInt = object : Comparable<Int> {
        override fun compareTo(other: Int): Int = 0
    }
    // This is the proper call of foo function, while T is not a subtype of Serializable
    foo<Comparable<Int>>(list, list, comparableInt)
}
```

On the other hand, if we consider such a type:

```Kotlin
interface SerializableList : List<Serializable>
```

And would like to generate constraints from types `List<T>` and `SerializableList`,
if we follow the algorithm, we will consider a pair `List<T>` and `List<Serializable>` as before.
But in this case, we are able 
to infer that `T :> Serializable` 
as we know that `Serializable` is an actual type argument of the runtime value's type projected on `List`.
Consequently, this value may be cast to `List<Serializable>` and `List<Any>` and nothing else.

Moreover, this also works for such a type:

```Kotlin
interface InvariantList<T> : List<T>
```

## Constraints resolution

This part is quite straightforward:

1. If any of the types in the constraint is a type parameter, then just record a constraints.
2. If any of the types is the temporal type variable, then record a bound for this variable. 
   And generate new transitive constraints from this variable. 
   (For new upper bound, generate constraints with all lower bounds and vice versa)
3. If both of the types are simple types, 
   then generate new constraints based on their type parameters.
   More precisely, we have to look at each type parameter position in each common supertype 
   and record a constraint according to position's variance.
   We also should track effectively invariant parameters in the same way as before.

### Special cases

#### Intersection types

If we would like to satisfy a constraint `A :> B & C` we result in a
disjoint constraints `A :> B | A :> C` which is not easy to solve. 
As I got from [this moment of presentation](https://youtu.be/VV9lPg3fNl8?t=1391), 
if they met the situation that leads to disjoint constraints, 
they just do not add such constraints. 
On the next slide, 
they said that if all-except-one of the disjoint constraints are unsatisfied, 
then we could process such a constraint.

#### Flexible types

For flexible types we have to follow their subtyping rules. 
[Explanation](https://github.com/JetBrains/kotlin/blob/master/spec-docs/flexible-java-types.md).
More presicely:

* $A :> {B, C} => A :> C$
* ${B, C} :> A => B :> A$
* $A = {B, C} => A :> C, B :> A$

# Detection of the unreachable code

To detect unreachable code, or conditions that are unsatisfiable, 
we have to run the same inference algorithm and then check if the inferred constraints are satisfiable.

The constraints are satisfiable if 
for each type parameter and temporal variable (representing a real type), 
there is at least one type satisfying all constraints.
To check this, we have to run the following algorithm:

1. Find all types that are the least common supertypes for all lowerbounds.
2. Check that any of those types is a subtype of all upperbounds.

If there is no such a type, then the constraints are unsatisfiable and condition is always false.

The simple, but incomplete approximation of this property is
to check whether all of the lowerbounds are subtypes of all of the upperbounds.

# Changes to the type checking

## New type of statements

We introduce a new type of statements collected by the data-flow analysis, called *type intersection*.
Type intersection is a set of types 
that are known to have a common value in the specific node of the control-flow graph.

The issue with such statements is that they are not eligible for intersection.
For example, in the following code:

```Kotlin
open class Box<T>
class BoxString : Box<String>()

interface ListString : List<String>

fun <T> foo(box: Box<T>, list: List<T>): T {
    if (...) {
        box as BoxString
        // We know that T = String here
    } else {
        list as ListString
        // We know that T :> String here
    }
    // We should know that T :> String here
}
```

We are able to infer that `T :> String` in the end of the function. 
As we could not easily intersect statements `[{List<T> & ListString}]` and `[{Box<T> & BoxString}]`, 
so we should store in the flow inferred bounds instead of the intersections.

## Local type checker state

Compared to smart-casts, which is inferring just another type for the variables,
inference of the new bounds affects the whole type-checking process.
To correctly use the inferred bounds, we have to incorporate them into the subtyping check.
Thus, we have to add a dependency from type-checker to the control-flow graph node which does not exist right now.

To demonstrate the problem, let's remember the following code:

```Kotlin
fun <A> Chart<A>.myDraw(chartData: A): Unit =
    when (this) {
        is PieChart -> {
            // chartData is PieData in this branch
            ... // modify
            draw(chartData)
        }
        else -> draw(chartData)
    }
```

inference of the bounds from one variable affects the type of another variable
which could not be easily represented in the current type-checker. 

## Proper processing of the expected type

To incorporate a new bound into the subtyping check is not enough as for example for when expression, 
we have such a hierarchy of nodes:

* *when* expression
   * *when* branch
     * *when* branch body

And we may infer the bounds only in the "*when* branch body" node, 
but would like to use them in the "*when* expression 
while checking conformance of the expected type to the provided one.

The current implementation checks the expected type in the place where it was generated, 
while to correctly take GADT inference into account, 
we have to check the expected type more often
and in case of success, replace the inferred type of the expression with the expected one.

## Case without an expected type

While to typecheck the expression with the expected type is not a problem and this code could be easily typechecked:

```Kotlin
fun <T> foo(v: Box<T>) {
    val t: T = when (v) {
        is BoxString -> "string"
        is BoxInt -> 1
    }
}
```

The following code could not be easily typechecked as we do not know the expected type of the expression:

```Kotlin
fun <T> foo(v: Box<T>): T {
    val t = when (v) {
        is BoxString -> "string"
        is BoxInt -> 1
    }
    return t
}
```

As we do not know the expected type of the expression, 
we could not cast `Int` or `String` in the respecting branches to the type `T`.
And at the moment we are trying to intersect them, 
we had already lost the information about the local bounds of T.

## Exhaustiveness check

The issue with the exhaustiveness check is that such a code is marked as non-exhaustive (Both K1 and K2):

```Kotlin
interface A
interface B

sealed interface I<T>
class IA : I<A>
class IB : I<B>

fun <T : A> f(i: I<T>) {
    when (i) {
        is IA -> TODO()
    }
}
```

And such a code is marked as correct in K2 and failed with incompatible types error in K1:

```Kotlin
interface A
interface B

sealed interface I<T>
class IA : I<A>
class IB : I<B>

fun <T : A> f(i: I<T>) {
    when (i) {
        is IA -> TODO()
        is IB -> TODO()
    }
}
```

So Kotlin for now does not check the exhaustiveness of the branches based on the type parameters.
But this feature is usually associated with the GADT inference.

To implement such a check, we may re-use the same algorithm as for the inference of unsatisfiable conditions.
In this case, we have to infer constraints for each unmatched classifier
and remove classifiers that are leading to unsatisfiable constraints. 

# Related features

## Bare types

Gadt inference would be useful to infer bounds for bare types as well. 
Bare types are the types with omitted type parameters.
They are designed to use in cases like this:

```Kotlin
fun <T> foo(l: Collection<T>) {
    when (l) {
        is List -> ... // l is List<T>
        is Set -> ... // l is Set<T>
    }
}
```

Type parameters of bare types are inferred from the type of the scrutinee.
Inference of the type parameters is highly related to the GADT inference.
We have to infer all the bounds for the type parameters of the cast type 
based on the information that the value is both of original type and cast type with all arguments as `*`.
As the algorithm has to replace the star projections with temporal type variables 
(to manage constraints like $B :> * :> A$),
it is also inferring the bounds for such variables.
The next step would be to encode the type parameters to satisfy inferred bounds.

## Smart casts

The other existing feature that could be positively affected by the GADT inference is the smart casts.
As the smart cast is natively collecting the information about the types of the value in the specific branch,
we could use this information to specialize them. 
More precisely, we could specialize the type parameters that could be placed instead of the star projections. 
For example, in such a case:

```Kotlin
interface A<in T, in V>
interface A1<in V> : A<Int, V>
interface A2<in T> : A<T, Int>

fun f(v: A1<*>) {
    val v1 : A2<Int> = when (v) {
        is A2<*> -> v
        else -> throw Exception()
    }
}
```

We could infer the type `{A1<Int> & A2<Int>}` instead of the current `{A1<*> & A2<*>}`.
And then this code could be successfully typechecked.

## Existential types

The other possible feature of the type system that could bring them all together is the existential types.
As the behaviour of the existential types is quite similar to the behaviour of the generic parameters, 
we are able to use the same algorithm to infer local bounds for them.
With such a feature, 
the bare types feature could be implemented as a syntactic sugar for the type with existential type parameters.
As well as we could replace all star projections with implicit existential type parameter, 
we could natively introduce refinement of the star projections in the smart-casts (and maybe slightly more).

# Breaking changes

## Resolution changes

As we will extend the type hierarchy, it may affect the resolution based on the type.
For example:

```Kotlin
interface A
interface B : A

interface Out<out T>
interface OutB : Out<B>

fun <T> f(b: B, out: Out<T>) {
    fun A.foo() = "A"
    fun T.foo() = "T"
    
    when (out) {
        is OutB -> {
            // we inferred that T :> B
            
            b.foo()
            // was: resolved to A.foo()
            // will be: resolved to A.foo() and T.foo(), ambiguity error
        }
        else -> TODO()
    }
}
```

As we only may infer new supertypes (and subtypes), 
it will never lead to the resolution changes, 
only to the new compilation errors. 
But it is not a big deal as
* One of the functions has to be a local function on the generic parameter, which is not a common case.
* It may be automatically fixed with the migration tool.

# Questions for implementation

1. TODO: [Scala bugs](https://github.com/lampepfl/dotty/issues?q=label%3Aitype%3Abug+label%3Aarea%3Agadt)

# References

1.  [Presentation of the implementation in Scala 3 from the typelevel summit](https://www.youtube.com/watch?v=VV9lPg3fNl8)

2.  [Outline of the implementation in Scala 3](https://dl.acm.org/doi/pdf/10.1145/3563342) 
    (Section 6.2)

3.  [First formalization for C#](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/gadtoop.pdf)
    (Section 5.1)

[//]: # (4.  [Dotty PR 1]&#40;https://github.com/lampepfl/dotty/pull/5736&#41;, )
[//]: # (    [Dotty PR 2]&#40;https://github.com/lampepfl/dotty/pull/6398&#41;)

# Examples

1.  For code showing the unsoundness of the GADTs in Scala 2:

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
