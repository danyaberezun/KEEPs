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

Unfortunately, *`Kotlin` has no support for generalized pattern matching while allowing one to actually define a `GADT`*.
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

***The paper presents a proposal how the current type-checker can be modified in order to cover this __gap__ in the language design by adding support for GADT inference.***

***Moreover, adding the mechanism to support for generalized pattern matching in type-checker also improves smart-casts behaviour and allows one to get rid of a number of unsafe casts in source code.*** (See next section for details)


## The Problem Scope (or Accompanying Benefits)

In general, GADT inference is associated with pattern-matching when we match a value of the sum type on one of their constructors and are able to specialize the type parameters of the sum type based on their instances in the specific constructor.
It is how the GADT inference works in the functional languages with their system of subtyping.
While it is not the case for languages with OOP-style subtyping as it is shown in 
[first formalization for C#](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/gadtoop.pdf) 
and Scala 3 implementation (see a **Bounds inference algorithm** section below for details).
In fact, we are able to run GADT inference at the moment we are identifying that there is a value in the program that has two types, i.e., sum type and type of the specific constructor in the case of functional languages, and two arbitrary types in the case of Kotlin.
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

The current implementation that collects such statements only for variables
(as temporary values are not eligible for smart casts) 
is making the last three examples ill-typed.
But since temporary values are outfiltered at the very last stage,
we can easily collect these statements for any values.
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

[Source and more examples](https://chrilves.github.io/posts/gadts_by_use_cases/)

### Runtime subtyping evidence

One easy, but beneficial, benefit of GADTs is expressing relations
about types like \" $A <: B$\" or \" $A = B$\":

```Kotlin
sealed interface EqT<A, B>{
  class Evidence<X> : EqT<X, X>
}

sealed interface SubT<A, B>{
  class Evidence<A, B : A> : SubT<A, B>
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
* Get comparability property as a boolean, enum parameter, or comparator.
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

The programmer has to explicitly cast data to PieData as he is sure that it is always successful. 
In this case, it is true and could be inferenced by gadt inference. 
Then code could become more type-safe and less verbose:

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

There are types that are known to be supertypes of some value denoted as $S_{i}$. 
*Type* consists of classifiers and their type parameters.
We can project a type on classifier and get the type parameters of that classifier for that type.

Let $R$ be a runtime type of the value. 
As we do not know the real type parameters of the runtime type, 
while we are projecting a runtime type on any classifier, we are initializing them with the fresh type variables.

Let's introduce an operation "project runtime type on classifier X upcasted from projection on Y".
In this operation, we are not generating a new type variable for each type parameter.
We are substituting the variables generated for classifier Y, 
while going up in the hierarchy of classifiers from Y to X.

The algorithm consists of the following steps:

1. If $S_{i}$ is an intersection type, 
   add all types in the intersection to the set of types $S_{i}$ 
   and process them as individual supertypes of the value.
2. Project runtime type $R$ on lowest classifiers of each $S_{i}$ and denote them as $R_{S_{i}}$.
3. As the runtime type is a subtype of all $S_{i}$, 
   its projections are also subtypes of the corresponding $S_{i}$, 
   so record the constraint $R_{S_{i}} :> S_{i}$ for the second phase.
4. For each classifier $S_{i,j}$ which is the lowest upper bound of $S_{i}$ and $S_{j}$, 
   project runtime type $R$ upcasted from $R_{S_{i}}$ and $R+{S_{j}}$ on $S_{i,j}$ 
   and denote them as $R^{S_{i,j}}_{S_{i}}$ and $R^{S_{i,j}}_{S_{j}}$.
5. Record the constraint $R^{S_{i,j}}_{S_{i}} = R^{S_{i,j}}_{S_{j}}$,
   where = means syntactic equality of types.

The last step of the algorithm justified by the following paragraph of the Kotlin's specification:

> the transitive closure S∗(T) of the set of type supertypes S(T : \(S_1\), . . . , \(S_m\)) = {\(S_1\), . . . , \(S_m\)} ∪ S(\(S_1\)) ∪ . . . ∪ S(\(S_m\))
> is consistent, i.e., does not contain two parameterized types with different type arguments.

As $R^{S_{i,j}}_{S_{i}}$ and $R^{S_{i,j}}_{S_{j}}$ are projections of the same runtime type on the same classifier,
they are equal to each other.

### Compared to Scala

The algorithm is quite different from Scala's algorithm and may infer bounds in more cases.
The main difference arises from the mentioned paragraph of the Kotlin's specification 
which allows to simplify and enhance the algorithm.

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

As a result, for the code like this:

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
consequently, $A$ would be `Any` and $B$ would be `Int` which does not satisfy $A <: B$.

### Examples

#### Simple example

Let's review the algorithm on the following example:

```Kotlin
interface Expr<T>
interface ExprInt(var v: Int) : Expr<Int>

fun <T> eval(e: Expr<T>): T =
  when (e) {
    is ExprInt -> e.v
  }
```

As an input of the algorithm, we have two supertypes of the value `v`: `ExprInt` and `Expr<T>`.

The demonstration of the algorithm is shown in the following diagram:

![](images/example_simple.png)

Numbers in the image denote the corresponding steps of the algorithm.
The upper part of the diagram shows the expected outcome of the second phase of the algorithm.
Let's review the algorithm step by step.

1. Not applicable.
2. Project runtime type on `Expr` and `ExprInt` and generate a fresh variable `R` for the type parameter of `Expr`.
3. Record the constraints $Expr<T> :> Expr<R>$ and $ExprInt :> ExprInt$.
4. Project runtime type on `Expr` upcasted from the corresponding projections and receive types $Expr<R>$ and $Expr<Int>$.
5. Record the constraint $Expr<R> = Expr<Int>$.

#### Several least common classifiers

Let's review the algorithm on the following example:

```Kotlin
interface Expr<T>
interface Tag<T>
interface TExpr<E, T> : Expr<E>, Tag<T>
interface ExprInt : Expr<Int>, Tag<String>

fun <E, T> eval(e: TExpr<E, T>): E = when (e) {
  is ExprInt -> e.value
}
```

As an input of the algorithm, we have two supertypes of the value `value`: `ExprInt` and `TExpr<E, T>`.

The demonstration of the algorithm is shown in the following diagrams:

![](images/example_several_least_common_classifiers_1.png)

![](images/example_several_least_common_classifiers_2.png)

Let's review the algorithm step by step.

1. Not applicable.
2. Project runtime type on `TExpr` and `ExprInt` 
   and generate a fresh variables `R1` and `R2` for the type parameters of `TExpr`.
3. Record the constraints $TExpr<E, T> :> TExpr<R1, R2>$ and $ExprInt :> ExprInt$.
4. Project runtime type on `Expr` upcasted from the corresponding projections 
   and receive types $Expr<R1>$ and $Expr<Int>$.
5. Record the constraint $Expr<R1> = Expr<Int>$.
4. Project runtime type on `Tag` upcasted from the corresponding projections 
   and receive types $Tag<R2>$ and $Tag<String>$.
5. Record the constraint $Tag<R2> = Tag<String>$.

### Special cases

#### Flexible types

For flexible types, we have to run the algorithm on their upper bound 
as it is the type that is guaranteed to be a supertype of the real type.

## Constraints resolution

This algorithm is quite similar to the resolution of the constraints' system during function call generics' inference.
Roughly speaking, this resolution works this way: 
reduce (simplify) complex constraints until all of them are simple enough.
(f.e. generic parameter is on one of the sides of the constraint).
Roughly speaking, the signature of the reduce function is:

```Kotlin
fun reduce(constraint: Constraint): Set<Constraint>
```

Let's denote the resulting set of constraints as $RCs$ and the input constraints as $C$.

The desired behavior of the reduce function is: $RCs ⟺ C$. 
Which means that the set of constraints $RCs$ is equivalent to the set of constraints $C$.
Consequently, if there is a solution for the original set of constraints, 
then there is a solution for the reduced set of constraints and vice versa.

Sometimes it is impossible to express the original constraint 
using the simpler ones due to constraint language limitations.
In these cases, the behavior of the reduce function is: $RCs ⟸ C$.
Which means that the set of constraints $RCs$ is stricter than the original constraint $C$.
Consequently, if there is a solution for the original set of constraints, 
then there is a solution for the reduced set of constraints, but not vice versa.
Such behavior allows to type-check only the correct function calls, while not all of them.

The difference between the resolution of the constraints' 
system during function call generics' inference and GADT inference 
is that in case we are not able to solve constraints precisely, we have to approximate them in the other direction.
It means, that the behavior of the reduce function have to be: $RCs ⟹ C$.
Which means that the set of constraints $RCs$ is less strict than the original constraint $C$.
Consequently, if there is some constraint on type parameter in the resulting set of constraints, 
then there is the same constraint in the original set of constraints, but not vice versa.

So to implement the resolution of the constraints, 
we have to adopt the existing resolution algorithm for the another approximation regime. 

### Examples

#### Another approximation

For example, if we are trying to reduce the constraint $S <: T$, where T is a type variable.

For the function call generics' inference, the result of the reduction may be $S <: LB(T)$, 
where $LB(T)$ is the lower bound of T.
This constraint will guarantee that the original constraint is satisfied.

For the GADT inference, the result of the reduction may be $S <: UB(T)$, 
where $UB(S)$ is the upper bound of S.
This constraint is guaranteed to be satisfied by the original constraint.

#### Intersection types

If we would like to satisfy a constraint `A :> B & C` we result in a
disjoint constraints `A :> B | A :> C` which is not easy to solve. 
As stated at [this moment of presentation](https://youtu.be/VV9lPg3fNl8?t=1391), 
if Scala 3 met the situation that leads to disjoint constraints, 
they just do not add such constraints. 
On the next slide, 
stated that in case if all-except-one of the disjoint constraints are unsatisfied, 
then such a constraint could be processed.

#### Flexible types

For flexible types, we have to follow their subtyping rules. 
[Explanation](https://github.com/JetBrains/kotlin/blob/master/spec-docs/flexible-java-types.md).
More precisely:

* $A :> \{B, C\} => A :> C$
* $\{B, C\} :> A => B :> A$
* $A = \{B, C\} => A :> C, B :> A$

## Special cases

### Projections

Projections will be successfully handled by the resolution algorithm.
To achieve this, they will be captured.
As a result, we will have a bounds for the type parameters that may contain captured types.
The issue there is that currently Kotlin uncapture projections exactly after the resolution of the system, 
so programmers do not interact with captured types directly.

For GADT bound, we are not able to uncapture all of them as it will lead to the unsound constraints.
Let's review the examples using the following definitions:

```Kotlin
class Inv<T>
class Cov<out T>
class Con<in T>
```

For example: 

1. If the algorithm produces us a constraint $T :> Con<(Captured(*))>$, 
   then this only provides us information that T is not nullable and $Con<T> :> Con<Con<Any?>>$.
   While if we uncapture it, 
   we will receive $T :> Con<*>$, which leads to, for example, $T :> Con<*> :> Con<Int>$, which is unsound.

2. If the algorithm produces us a constraint $T :> Con<Inv<Captured(*)>>$, 
   we will be able to approximate this constraint to $T :> Con<Inv<*>>$

3. On the contrary, for the upper bounds,
   we are able to uncapture the constraint $T <: Con<(Captured(*))>$ but not $T <: Con<Inv<Captured(*)>>$.

The idea is that by uncapturing the type with the captured type as a parameter, we are generalizing this type.
So we may do it only if it relaxes the constraint.

The information that we are loosing by this relaxation 
is the equalities between the captured types (or existential variables).
For example, if we have a constraints $V = Inv<(Captured(*))>$ and $U = Inv<(Captured(*))>$,
where the captured types are the same, 
we will be able to call a function `fun <T> foo(i1: Inv<T>, i2: Inv<T>)` with the values of types `V` and `U`.
And these equalities are required 
to write a code with the structures that are controlling some invariants on the type level, 
for example, AVL tree with type-level control of the balance factor.

The possible solutions are
1. Erase all bounds containing captured types that could be soundly uncaptured.
   This option will significantly limit the applicability of the GADT inference.
2. Remain the captured types in the bounds.
   This option will complicate the printing of types to the user 
   as currently captured types are not supposed to be printed.
   But this option will significantly increase the applicability of the GADT inference 
   and allow to strengthen the captured types in other parts of the type system as well 
   and make them closer by expressiveness to the existential types.

# Changes to type checking

## New type of statements

We introduce a new type of statements collected by the data-flow analysis, called *type intersection*.
Type intersection is a set of types 
that are known to have a common value in the specific node of the control-flow graph.

The issue with such statements is that they are not eligible for intersection of flows.
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

inference of the bounds from one variable affects the type of another variable, 
which could not be easily represented in the current type-checker. 

## Proper processing of the expected type

To incorporate a new bound into the subtyping check is not enough as, for example, for when expression, 
we have such a hierarchy of nodes:

* *when* expression
   * *when* branch
     * *when* branch body

And we may infer the bounds only in the "*when* branch body" node, 
but would like to use them in the "*when* expression 
while checking the conformance of all the branches to the expected type.

The current implementation checks the expected type in the place where it was generated, 
while, to correctly take GADT inference into account, 
we have to check the expected type more often
and in case of success, replace the inferred type of the expression with the expected one.

## Case without an expected type

While to typecheck the expression with the expected type is not a problem, and this code could be easily typechecked:

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

## Overload resolution

The main place where the absence of an expected type arises is in arguments of a function call.
Due to type-based resolution, the arguments of a call do not actually have an expected type.

Let's consider the following example:

```kotlin
sealed interface Box<T>
class BoxInt : Box<Int>

fun foo(a: Any?) = println("Any?")
fun foo(s: String) = println("String")
fun foo(i: Int) = println("Int")

fun <T> bar(t: T, b: Box<T>) = foo(when (b) {
    is BoxInt -> t
})
```

One may expect that the type of the argument of `foo`'s call in will be resolved into 
`Int` and the corresponding `foo`'s overload will be called, resulting in printing "Int".
Unfortunately, this is not the case.
Since the actual expected type is unknown, 
the inferred type for the expression in the argument is `T`, 
and call resolves the overload with `Any?`.
Even more, if we completely remove this overload, 
we will get a compilation error as none of the overloads is applicable.
If we then remove the overload with `String`, 
then `foo` will be successfully resolved to the only remaining declaration of `foo`.
This happens because in case we only have one overload, we can use it to get the expected type, 
and with the help of this type we are able to infer the type `Int` for this argument.

The real problem here is that we do not actually choose the most specific overload for a given expression, 
but rather for arguments' type, which may be inferred in different ways.
This behaviour may be confusing for the programmers, so it should be explicitly documented.

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
As the algorithm has to replace the star projections with temporary type variables 
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

## Dead code detection

To detect unreachable code, or conditions that are unsatisfiable,
we have to run the same inference algorithm and then check if the inferred constraints are satisfiable.

The constraints are satisfiable if
for each type parameter and temporary variable (representing a real type),
there is at least one type satisfying all constraints.
To check this, we have to run the following algorithm:

1. Find all types that are the least common supertypes for all lowerbounds.
2. Check that any of those types is a subtype of all upperbounds.

If there is no such a type, then the constraints are unsatisfiable and condition is always false.

The simple, but incomplete approximation of this property is
to check whether all of the lowerbounds are subtypes of all of the upperbounds.

### Exhaustiveness check

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
