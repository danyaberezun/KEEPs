# Subtype reconstruction aka GADT-style inference

## Introduction
### (Generalized) algebraic data types 

Kotlin currently allows one to declare algebraic data types, or *ADT*, via sealed classes/interfaces and data classes.
ADTs allows one to form a type by combining other types, where the sum type is represented as a sealed class/interface, and the product type is represented as a data class.
The beauty comes when ADTs are also equipped with pattern matching (`when` expressions) on their structure and types.

In functional programming (for example, with languages like Scala, OCaml, Haskell) there is a more powerful concept of generalized algebraic data types, or *GADT* (aka guarded recursive datatype).
It is a generalization of parametric ADTs which permits value constructors to return specific, rather than parametric, type-instantiations of their own datatype.

> TODO: add a simple example of ADT vs GADT which shows what is the difference between specific and parametric type instantiations.

GADTs enable the storage of additional type information (invariants) in ADTs, along with the ability of using this information when doing pattern matching.

One of the classic well-known GADT use-cases is ensuring type safety when defining DSLs.
For example, in Scala an arithmetic expression can be made type-safe by construction, e.g. it is impossible to construct an expression that uses binary operator on non-numerical values and tuples.

```Scala 
enum Expr[A]:
  case LitInt(i: Int) extends Expr[Int]
  case Add(e1: Expr[Int], e2: Expr[Int]) extends Expr[Int]
  case Tuple[X, Y](x: Expr[X], y: Expr[Y]) extends Expr[(X, Y)]
```

In the example constructor `LitInt` ensures that the data being created is an `Expr[Int]`, not just some generalized `Expt[T]`, while binary addition constructor `Add` checks that its sub-expressions are numbers, i.e., are of type `Expr[Int]`.
Thus, in this case our *data type invariants* are: any integer literal is actually an integer, and any binary addition has integer sub-expressions.
This information is stored in the type itself, meaning there is no way to construct an ill-formed expression (for example, a binary addition of two tuples).

As ADTs comes with pattern matching, generalized ADTs come with generalized pattern matching which uses the type information stored in GADTs to guarantee code type safety.
In other words, as GADTs represent *types correct by construction at compile-time*, generalized pattern matching *guarantees absence of type errors during run-time*.

Continuing on the previous example, the following function that evaluates arithmetic expressions is well-typed.

```Scala
def eval[T](e: Expr[T]): T = e match
  case LitInt(i) => i // GADT constraint `T = Int` allows the branch
                      // to return an `Int` and not a generic `T`
  case Add(e1, e2) => eval(e1) + eval(e2)
                      // GADT constraint allows to infer that `eval(Expr[Int])`
                      // returns an `Int` and not a generic `T`,
                      // which allows to use binary addition
  case Tuple(x, y) => (eval(x), eval(y))
```

That is, we locally use the information stored in GADTs in specific branches of pattern matching, ensuring the branch is well-typed with respect to this information, and correctly "forget" the information outside of these branches.
Without the ability to do generalized pattern matching, GADTs lose most of their safety and expressive power.

As follows from these points, GADTs are most useful in applications where compile- and run-time type safety is especially important, for example, complex DSLs, strongly-typed evaluators, generic data structure pretty-printing, traversals and queries, or database access.

### GADTs in Kotlin

Unfortunately, Kotlin allows one to define a GADT, but it has *no support for generalized pattern matching*.
This means that Kotlin users either have to avoid using GADTs (i.e., it's as if Kotlin does not support GADTs) or preserve and validate the data type invariants *by hand* (i.e., the users have to write boilerplate code with the possibility of making an error).

For example, if we translate the Scala example from above to Kotlin, it does not type check, because the Kotlin type checker is not able to infer that `e.i` is of type `Int`, even though in principle all necessary type information is there.

```Kotlin
sealed class Expr<out T>
data class ExprIntLit(val i: Int) : Expr<Int>()

fun <T> eval(e: Expr<T>): T = when (e) {
    is ExprIntLit -> e.i // Type mismatch. Required: T, Found: Int
}
```

```Kotlin
interface Expr<out T>
class ExprIntLit(val i : Int) : Expr<Int>()

fun <T> eval(e: Expr<T>): T = when (e) {
    is ExprIntLit -> e.i // Type mismatch. Required: T, Found: Int
    else -> TODO()
}
```

> TODO: do we need the second example? What new information w.r.t. GADTs does it show?

This KEEP proposes how the current Kotlin type system can be modified in order to cover this problem in the language design by adding support for generalized pattern matching.

Besides improvements to the GADT user experience, adding the support for generalized pattern matching also improves smart casts behaviour and allows one to get rid of a number of unsafe casts in the code even without the use of GADTs.

## From generalized pattern matching to subtype reconstruction

As we have established, GADTs are associated with generalized pattern matching: when we match a value of GADT on one of its variants, we can infer precise type arguments for this value based on the type arguments in the matched GADT variant.
It is how the GADTs already works in many functional languages, but for object-oriented languages with inheritance-based subtyping it is not as easy and actually not enough to achive only generalized pattern matching.

> The details of why it is so could be found in [the GADT formalization for C#](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/gadtoop.pdf) and Scala 3 implementation.
> Additionally, you could see the [Bounds inference algorithm](...) section of this KEEP.

In fact, in general you have access to additional type information when there is a value in the program that has two related types.
For functional languages, this happens, for example, with GADT and one of its specific variant type in pattern matching; for object-oriented languages, this can happen for two arbitrary types, when one is inherited from (is a subtype of) another.

This means for Kotlin we would need to have something more general than generalized pattern matching, something which allows to use the subtyping information anywhere.
We will call this *"subtype reconstruction"*, as its core idea is to reconstruct additional subtyping information for related types.

Kotlin has a flow-sensitive type system, in order to support smart casts, i.e. it collects information about possible types of values in a flow-sensitive way, meaning it can track that a value has two possible types and it can know if these types are related.
Since this information is not limited to `when`-expressions, it allows us to have general subtype reconstruction which is not limited to pattern matching only.

For example, the four examples below are actually well-typed as in all these cases we know that `e` is of both `ExprIntLit` and `Expr<T>` types, which means that `T = Int`.

```Kotlin
sealed class Expr<out T>
data class ExprIntLit(val i: Int) : Expr<Int>()

fun <T> evalWhen(e: Expr<T>): T = when (e) {
    is ExprIntLit -> e.i
}

fun <T> evalIs(e: Expr<T>): T {
    if (e is ExprIntLit) {
        return e.i
    }
    TODO()
}

fun <T> evalNotIs(e: Expr<T>): T {
    if (e !is ExprIntLit) {
        TODO()
    }
    return e.i
}

fun <T> evalEquality(e1: Expr<T>, e2: ExprIntLit): T {
    if (e1 === e2) {
        return e1.i
    }
    TODO()
}
```

> TODO: previously we said that only the last three examples do not work, but for me all four examples have RETURN_TYPE_MISMATCH.
> Could we double check what happens here?

The current type system implementation collects such statements only for separate variables, which makes these examples ill-typed.
Let us explain how we can extend the Kotlin type system to support subtype reconstruction.

## Subtype reconstruction

### Bounds inference algorithm

The subtype reconstruction is based on bounds inference algorithm.
The purpose of this algorithm is to infer bounds for type arguments based on the extended subtype information available from Kotlin flow-sensitive type system.

As an input, the algorithm accepts a set of types `T1 & T2 & ...` for a specific value which this value definitely has at a specific program point.
As an output, it infers additional bounds for type arguments used in types `T1 & T2 & ...`.
The algorithm consists of two parts, generation of subtyping and equality constraints, and their resolution.

### Generation of constraints

The pseudocode for the constraint generation is shown below.

```Kotlin
fun generateConstraintsFor(supertypes: List<Type>) {
  val assumptions = List<Assumption>()
  val projections = supertypes.map {
    createRealTypeProjection(it.classifier)
  }
  supertypes.zip(projections).forEach {
    supertype, projection ->
      assumptions.add(projection <: supertype)
  }
  projections.cartesianProduct().forEach { proj1, proj2 ->
    val lowestCommonClassifiers: List<Classifier> = lcc(
      proj1.classifier, proj2.classifier)
    lowestCommonClassifiers.forEach { classifier ->
      val upcastedProj1 = upcast(proj1, classifier)
      val upcastedProj2 = upcast(proj2, classifier)
      assumptions.add(upcastedProj1 =:= upcastedProj2)
  }
}
```

> TODO: this needs to be carefully re-read to check I didn't break smth when rewriting the algorithm.

The input for the algorithm is a list of known supertypes for some value, which come from the compile-time information in the code (type declarations, type checks, etc.).

Stage 1: If these supertypes contain intersection types, we consider each of the intersection type components as a separate supertype.

Stage 2: Next, in line 3, we create so called "type projections" of these supertype.
A type projection of a supertype is this type's classifier type parameterized with fresh type arguments (if any).
It can be viewed as a placeholder for the actual runtime type of the value.

Stage 3: Then, in line 6, we record the constraint that these type projections are subtypes of their corresponding supertypes, as the actual runtime type of the value will be a subtype of its compile-time checked supertype.

Stage 4: After that, in line 10, we iterate over all lowest common classifiers (line 14) for each possible pair of the type projections.
The lowest common classifiers are determined with respect to the inheritance relation.
Then, in line 14, we upcast both projections on all of those classifiers. Upcasting is the process of "lifting" the subtype to its supertype along the inheritance hierarchy together with the substitution of the type parameters.

Stage 5: Finally, we generate strict equalities between these upcasted projections, as they represent supertypes of the same type (real type of the considered value) w.r.t. the same classifier.
This is justified by the following paragraph of the Kotlin specification.

> The transitive closure S∗(T) of the set of type supertypes S(T : \(S_1\), . . . , \(S_m\)) = {\(S_1\), . . . , \(S_m\)} ∪ S(\(S_1\)) ∪ . . . ∪ S(\(S_m\))
> is consistent, i.e., does not contain two parameterized types with different type arguments.

#### Examples

##### Simple example

Let's review the algorithm on the following example.

```Kotlin
interface Expr<T>
interface ExprInt(var v: Int) : Expr<Int>

fun <T> eval(e: Expr<T>): T =
  when (e) {
    is ExprInt -> e.v
  }
```

As an input for the algorithm, we have two supertypes of the value `e`: `ExprInt` and `Expr<T>`.

The flow of the algorithm is shown in the following diagram.

![](images/example_simple.png)

The upper part of the diagram shows the final generated constraints.
Let's follow the algorithm step by step.

* Stage 1. Not applicable.
* Stage 2. Do type projection on `Expr<T>` to get `Expr<R>` (where `R` is a fresh type variable) and on `ExprInt` to get `ExprInt`.
* Stage 3. Record the constraints $Expr<T> :> Expr<R>$ and $ExprInt :> ExprInt$.
* Stage 4. For the lowest common classifier `Expr`, upcast the corresponding projections and get types $Expr<R>$ and $Expr<Int>$.
* Stage 5. Record the constraint $Expr<R> =:= Expr<Int>$.

##### Example with several lowest common classifiers

Let's review the algorithm on the following, more complicated example.

```Kotlin
interface Expr<T>
interface Tag<T>
interface TExpr<E, T> : Expr<E>, Tag<T>
interface ExprInt : Expr<Int>, Tag<String>

fun <E, T> eval(e: TExpr<E, T>): E = when (e) {
  is ExprInt -> e.value
}
```

As an input of the algorithm, we have two supertypes of the value `e`: `ExprInt` and `TExpr<E, T>`.

The flow of the algorithm is shown in the following diagrams:

![](images/example_several_least_common_classifiers_1.png)

![](images/example_several_least_common_classifiers_2.png)

Let's follow the algorithm step by step.

* Stage 1. Not applicable.
* Stage 2. Do type projection on `TExpr<E, T>` to get `TExpr<R1, R2>` (where `R1` and `R2` are fresh type variables) and `ExprInt` to get `ExprInt`.
* Stage 3. Record the constraints $TExpr<E, T> :> TExpr<R1, R2>$ and $ExprInt :> ExprInt$.
* Stage 4.
    * For the lowest common classifier `Expr`, upcast the corresponding projections and get types $Expr<R1>$ and $Expr<Int>$.
    * For the lowest common classifier `Tag`, upcast the corresponding projections and get types $Tag<R2>$ and $Tag<String>$.
* Stage 5.
    * Record the constraint $Expr<R1> =:= Expr<Int>$.
    * Record the constraint $Tag<R2> =:= Tag<String>$.

#### Special cases

* Flexible types. For flexible types, we have to run the algorithm on their upper bound, as it is the type that is guaranteed to be a supertype of the real type.

> TODO: Maybe add an example of how the algorithm works for flexible types?

#### How this compares to the Scala GADT algorithm?

The algorithm is quite different from the Scala GADT algorithm and may infer bounds in more cases.
The main difference arises from the mentioned paragraph of the Kotlin specification, aka supertype set consistency, which allows to simplify and enhance the algorithm.

For instance, the following code:

```Scala
trait Func[-A, +B]
trait Identity[X] extends Func[X, X]
trait FalseIdentity extends Identity[Int], Func[Any, Int]
```

is valid in Scala, while the same code in Kotlin:

```Kotlin
interface Func<in A, out B>
interface Identity<X> : Func<X, X>
interface FalseIdentity : Identity<Int>, Func<Any, Int>
```

fails to compile with error: `Type parameter B of 'Func' has inconsistent values: Int, Any`.

As a result, for the code like this:

```Kotlin
fun <A, B> foo(func: Func<A, B>) = when (func) {
    is Identity<*> -> {
        val b: B = mk() as A
    }
    else -> TODO()
}
```

we are able to infer relation $A <: B$ in Kotlin.

However, this is not a case for Scala, as there we may have a value of `FalseIdentity` type, for which $A$ would be `Any` and $B$ would be `Int`, and these do not satisfy $A <: B$.

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

Compared to smart casts, which is inferring just another type for the variables,
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
we could natively introduce refinement of the star projections in the smart casts (and maybe slightly more).

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
    to infer $S_{real}$ due to absence of variance in $D$.

## Addendum

### Prototype

[Prototype implementation](https://github.com/e2e4b6b7/kotlin/pull/2)

### GADT use-cases

[Sources and more examples](https://chrilves.github.io/posts/gadts_by_use_cases/)

#### Runtime subtyping evidence

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

#### Type-safe extensions

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

### Real-world GADT-like examples

1. [GitHub link](https://github.com/JetBrains/kotlin/blob/242c1cf5f0814fbe9df02b4b85a63298b30b4b67/core/reflection.jvm/src/kotlin/reflect/jvm/internal/calls/ValueClassAwareCaller.kt#L45)
2. [GitHub link](https://github.com/JetBrains/kotlin/blob/242c1cf5f0814fbe9df02b4b85a63298b30b4b67/compiler/resolution/src/org/jetbrains/kotlin/resolve/calls/KotlinCallResolver.kt#L165)
3. [GitHub link](https://github.com/JetBrains/kotlin/blob/242c1cf5f0814fbe9df02b4b85a63298b30b4b67/compiler/fir/providers/src/org/jetbrains/kotlin/fir/types/TypeUtils.kt#L211-L21)
4. [GitHub link](https://github.com/JetBrains/kotlin/blob/242c1cf5f0814fbe9df02b4b85a63298b30b4b67/jps/jps-plugin/src/org/jetbrains/kotlin/jps/model/ProjectSettings.kt#L72-L75)

### Real-world GADT-like use-cases

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

> TODO: what exactly are these Sources about and how they are different, for example, from the references above?

* https://github.com/higherkindness/mu-scala
* https://github.com/AdrielC/free-arrow
* https://github.com/milessabin/shapeless
* https://github.com/owlbarn/owl
* http://www.cs.nott.ac.uk/~psznhn/Publications/icfp2005.pdf
* http://gallium.inria.fr/~fpottier/publis/fpottier-regis-gianas-typed-lr.pdf
* http://pauillac.inria.fr/~fpottier/slides/slides-popl04.pdf
* https://www.cs.tufts.edu/~nr/cs257/archive/tim-sheard/lang-of-future.pdf
* https://infoscience.epfl.ch/record/98468/files/MatchingObjectsWithPatterns-TR.pdf
