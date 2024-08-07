# Return type inference with subtyping reconstruction

## Problem statement

The issue with return type inference is backed by the fact 
that we lose the local context where the value was created before we merge values from different branches.
Additionally, we do not have this context at the moment this value assigned to the variable.
More precisely, for this code:

```Kotlin
fun <T> foo(b: Box<T>) {
    val x = when (b) {
        is BoxString -> "string"
        is BoxInt -> 1
    }
    TODO()
}
```

The data flow is following:

```
assign
whenExpression
whenScrutineeExpression
whenBranchCondition
whenBranchExpression
todoExpression
```

While the order of the execution is following:

```
whenScrutineeExpression
whenBranchCondition
whenBranchExpression
whenExpression
assign
todoExpression
```

> When expression before assign is important as we may not have an assignment, but use a return type of the whenExpression, which have to be inferred. For example, if the function argument is whenExpression itself

In this order we do not lose bounds information, 
we merge it at the same moment as we merge a return value types. 
Actually, we would like to transfer a return type through the data flow graph, 
along with the inferred bounds allowing us to merge them simultaneously.
It does not mean they have to be stored in the DFG instead of the FIR node itself, 
but it has to be propagated through the DFG and assigned into FIR after the DFG passed this node.

> It does not have to be implemented this way, 
> we may just (temporarily) store the context of the return type along with it in the FIR, 
> but we have to track if the order, etc. aligned with the theoretical model.

So actually with this new order, 
we have a place where we have to merge the return type and the bounds simultaneously.

## Algorithm

We have to define three new basic operations:

1. $lub(T_1, T_2, C_1, C_2)$, where
   * $T_1$ and $T_2$ are types to merge
   * $C_1$ and $C_2$ are constraints from the corresponding branches

   The (desired) semantics of this operation is the following:

   $\forall T. (C_1 \vdash T :> T_1) \\& (C_2 \vdash T :> T_2) <=> \vdash T :> lub(T_1, T_2, C_1, C_2)$

   > Emptiness of the context on the right side is not required.
   > Actually, we may say that we have a context $C_1 \\& C_2$ there.
   > But to construct this context, we need this operation itself.
   > For optimization of the resulting type we may use any context, which is a subset of $C_1 \\& C_2$.
   > So we may firstly construct such context using bounds that are presented in both contexts.
   > It will prevent us from duplicating types from the same context bounds on every consecutive merge.
   > But for simplicity, we assume that we later shrink the type using the constructed context.

   The operation is used to merge the return type itself and type parameters in covariant positions.
2. $glb(T_1, T_2, C_1, C_2)$, where
   * $T_1$ and $T_2$ are types to merge
   * $C_1$ and $C_2$ are constraints from the corresponding branches

   The (desired) semantics of this operation is the following:

   $\forall T. (C_1 \vdash T <: T_1) \\& (C_2 \vdash T <: T_2) <=> \vdash T <: lub(T_1, T_2, C_1, C_2)$

   The operation is used to merge type parameters in contravariant positions.
3. $eq(T_1, T_2, C_1, C_2)$, where
   * $T_1$ and $T_2$ are types to merge
   * $C_1$ and $C_2$ are constraints from the corresponding branches
   
   The output of this operation is a __set__ of types. 

   The (desired) semantics of this operation is the following:

   $\forall T. (C_1 \vdash T = T_1) \\& (C_2 \vdash T = T_2) <=> \vdash T \in lub(T_1, T_2, C_1, C_2)$
   
   The operation is used to merge type parameters in invariant positions.

### Basic operations

#### $lub$

1. Find all supertype constructors of $T_i$ in $C_i$ that are the lowest upper bounds of their intersection in empty (or optimistic) context.
   Where constructor is not only a classifier but also a (generic) variable.
2. For classifiers from the previous step, find the appropriate bounds for their type parameters
   (recursively, using function according to accumulated variance).
3. Intersect the results.
4. Add union of $T_i$ in the result if its approximation is not already a supertype of the result.
5. \* Shrink the result using the context $C_1 \\& C_2$.

#### $glb$

1. Find all subtype constructors of $T_i$ in $C_i$ that are the greatest lower bounds of their union in empty (or optimistic) context.
   Where constructor is not only a classifier but also a (generic) variable.
   > Actually, we do not try to iterate over subtyping hierarchy, we use only subtyping introduced by context.
2. For classifiers from the previous step, find the appropriate bounds for their type parameters
   (recursively, using function according to accumulated variance).
3. Unite the results.
4. Add intersection of $T_i$ in the result if it is not already a subtype of the result.
5. \* Shrink the result using the context $C_1 \\& C_2$.

#### $eq$

1. Find all constructions equal to $T_i$ in $C_i$ and intersect these sets.
2. For classifiers from the previous step, find the appropriate bounds for their type parameters
   (recursively, using function according to accumulated variance (once invariant => forever invariant)).
3. \* Shrink the result using the optimistic context.

#### Handling of unions and non-singleton sets for invariant positions

##### Unions

During $glb$ operation we may end up with a union that we have to approximate to its subtype.
The most precise sound approximation is to select one of the components from the union.
But it is too imprecise and not obvious which one to select.
Observations:

1. Which one to choose:
   1. If the component from the stage 4, was added into union, we have to choose it.
      Firstly, for backward compatibility.
      Secondly, as it is most user-friendly.
   2. If there is no such component, but it is not equal to `Nothing`, we have to choose component of the union that is a supertype of it.
      It is the most precise backward compatible approximation.
   3. If there is no such component, and it is equal to `Nothing` we may choose any component.
2. Another approach is to transform the type into an equivalent one, with intersection instead of union.
   For example, $(T | V) \rightarrow Unit = (T) \rightarrow Unit \\& (V) \rightarrow Unit$
   Problem with this approach is that it make the inferred type less user-friendly and may result in a very big intersection (f.e. $(T | V, T | V, T | V) \rightarrow Unit$).
   So if it is even applicable, we are able to use it in some cases, when there is only one such parameter.
3. Should we choose?
   We may default to the most user-friendly option ($\\& T_i$) in case of any ambiguity.
   And add inspection to the IDEA "GADT inference failed, you should specify type explicitly". 
   And show options from unions in the hint.
   Actually, it should not be a lot of such cases in the real code.

##### Invariant position with non-singleton sets

Approach from point 2 from a previous section is also applicable here.
With the same disadvantages.

And the best solution to prevent explosion of the intersection is to default to `*`.
With the same inspection in IDEA.

> Actually, not. Our hypothesis is shown at the end of the document. 
   
### Operations

#### Flow union

1. For each type variable $V$.
2. Upper bound.
   1. $T_i$ is an intersection of upper bounds not presented for $V$ in any $C_j$, where $i != j$
      (Common bounds goes directly into optimistic context)
   2. Upper bound for V in merged context is $lub(T_i, C_i)$, plus common upper bounds.
3. Lower bound.
   1. $T_i$ is a union of lower bounds not presented for $V$ in any $C_j$, where $i != j$
        (Common bounds goes directly into optimistic context)
   2. Lower bound for V in merged context is $glb(T_i, C_i)$, plus common lower bounds.
4. Equality.
   For equality, we have to intersect all equivalency classes in all contexts.

#### Return type union

1. Unite the flows to get a precise next context for the return type. 
2. $lub$ of the return types from all branches with precise next context instead of empty or optimistic.

### Examples

#### Covariance with single generic

```Kotlin
fun <T> foo(v: Out<T>) {
    val v = when (v) {
        is OutString -> "string"
        is OutInt -> 1
    }
    // ...
}
```

Input:
- $T_1 = String; C_1 = \\{T :> String\\}$
- $T_2 = Int; C_2 = \\{T :> Int\\}$

Calculations:
- $glb(String, Int, C_1, C_2) = Nothing$
- $lub(String, Int, C_1, C_2) = T \\& Serializable \\& Comparable\langle Nothing\rangle$
  1. $\\{T, Comparable, Serializable\\}$
  2. $Comparable\langle glb(Int, String, C_1, C_2)\rangle \rightarrow Comparable\langle Nothing\rangle$
  3. $T \\& Serializable \\& Comparable\langle Nothing\rangle$
  4. $Int | String \rightarrow Serializable \\& Comparable\langle Nothing\rangle$ => do not add

Output:
- $T :> glb(String, Int, C_1, C_2) = Nothing$
- $RT = lub(String, Int, C_1, C_2) = T \\& Serializable \\& Comparable\langle Nothing\rangle$

#### Contravariance with single generic

```Kotlin
fun <T> foo(v: In<T>) {
    val v = when (v) {
        is InString -> { arg: String -> v.in(arg) }
        is InInt -> { arg: Int -> v.in(arg) }
    }
    // ...
}
```

Input:
- $T_1 = String; C_1 = \\{T <: String\\}$
- $T_2 = Int; C_2 = \\{T <: Int\\}$

Calculations:
- $glb(String, Int, C_1, C_2) = T$
  1. $\\{T\\}$
  2. Skip
  3. $T$
  4. $String \\& Int = Nothing$ => do not add
- $lub(String, Int, C_1, C_2) = Serializable \\& Comparable\langle T\rangle$

Output:
- $T <: lub(String, Int, C_1, C_2) = Serializable \\& Comparable\langle T\rangle$
- $RT = Function<glb(String, Int, ...), Unit> = Function<T, Unit>$

> Here we forgot that $T <: String => T = String$.
> See next example for such case.

> If none of $String$, $Int$ is final, then:
> - $glb(String, Int, C_1, C_2) = Int \\& String$
> - $RT = Function<Int \\& String, Unit>$

#### Invariance with single generic

```Kotlin
fun <T> foo(v: Inv<T>) {
    val v = when (v) {
        is InvString -> "string"
        is InvInt -> 1
    }
    // ...
}
```

Input:
- $T_1 = String; C_1 = \\{T = String\\}$
- $T_2 = Int; C_2 = \\{T = Int\\}$

Calculations:
- $glb(String, Int, C_1, C_2) = T$
  1. $\\{T\\}$
  2. Skip
  3. $T$
  4. $String \\& Int = Nothing$ => do not add
- $lub(String, Int, C_1, C_2) = T \\& Serializable \\& Comparable\langle T\rangle$
  1. $\\{T, Comparable, Serializable\\}$
  2. $Comparable\langle glb(Int, String, C_1, C_2)\rangle \rightarrow Comparable\langle T\rangle$
  3. $T \\& Serializable \\& Comparable\langle T\rangle$
  4. $Int | String \rightarrow Serializable \\& Comparable\langle Nothing\rangle$ => do not add

Output:
- $T <: lub(String, Int, C_1, C_2) = Serializable \\& Comparable\langle T\rangle$
- $T :> glb(String, Int, C_1, C_2) = T$
- $RT = lub(String, Int, C_1, C_2) = T \\& Serializable \\& Comparable\langle T\rangle = T$

#### Covariance with multiple generics

```Kotlin
fun <T, V> foo(t: Out<T>, v: Out<V>) {
    val v = when {
        t is OutString && v is OutString -> "string"
        t is OutInt && v is OutInt -> 1
        else -> error("")
    }
    // ...
}
```

Input:
- $T_1 = String; C_1 = \\{T :> String, V :> String\\}$
- $T_2 = Int; C_2 = \\{T :> Int, V :> Int\\}$

Calculations:
- $glb(String, Int, C_1, C_2) = Nothing$
- $lub(String, Int, C_1, C_2) = T \\& V \\& Serializable \\& Comparable\langle Nothing\rangle$
  1. $\\{T, V, Comparable, Serializable\\}$
  2. $Comparable\langle glb(Int, String, C_1, C_2)\rangle \rightarrow Comparable\langle Nothing\rangle$
  3. $T \\& V \\& Serializable \\& Comparable\langle Nothing\rangle$
  4. $Int | String \rightarrow Serializable \\& Comparable\langle Nothing\rangle$ => do not add

Output:
- $T :> glb(String, Int, C_1, C_2) = Nothing$
- $V :> glb(String, Int, C_1, C_2) = Nothing$
- $RT = lub(String, Int, C_1, C_2) = T \\& V \\& Serializable \\& Comparable\langle Nothing\rangle$

#### Contravariance with multiple generics

```Kotlin
fun <T, V> foo(t: In<T>, v: In<V>) {
    val v = when {
        v is InString && t is InString -> { arg: String -> v.in(arg) }
        v is InInt && t is InInt -> { arg: Int -> v.in(arg) }
        else -> error("")
    }
    // ...
}
```

Input:
- $T_1 = String; C_1 = \\{T <: String, V <: String\\}$
- $T_2 = Int; C_2 = \\{T <: Int, V <: Int\\}$

Calculations:
- $glb(String, Int, C_1, C_2) = T | V$
  1. $\\{T, V\\}$
  2. Skip
  3. $T | V$
  4. $String \\& Int = Nothing$ => do not add
- $lub(String, Int, C_1, C_2) = Serializable \\& Comparable\langle T | V\rangle$
  1. $\\{T, V, Comparable, Serializable\\}$
  2. $Comparable\langle glb(Int, String, C_1, C_2)\rangle \rightarrow Comparable\langle T | V\rangle$
  3. $T \\& V \\& Serializable \\& Comparable\langle T | V\rangle$
  4. $Int | String \rightarrow Serializable \\& Comparable\langle Nothing\rangle$ => do not add

Output:
- $T <: lub(String, Int, C_1, C_2) = Serializable \\& Comparable\langle T | V\rangle$
- $V <: lub(String, Int, C_1, C_2) = Serializable \\& Comparable\langle T | V\rangle$
- $RT = Function<glb(String, Int, ...), Unit> = Function<T | V, Unit>$
  - $Function<T | V, Unit> \rightarrow Function<T, Unit> \\& Function<V, Unit>$
  - or $Function<T | V, Unit> \rightarrow Function<*, Unit>$ + IDE warning

#### Invariance with multiple generics 1

```Kotlin
fun <T, V> foo(t: Inv<T>, v: Inv<V>) {
    val v = when {
        t is InvString && v is InvString -> Inv("string")
        t is InvInt && v is InvInt -> Inv(1)
        else -> error("")
    }
    // ...
}
```

Input:
- $T_1 = String; C_1 = \\{T = String, V = String\\}$
- $T_2 = Int; C_2 = \\{T = Int, V = Int\\}$

Calculations:
- $glb(String, Int, C_1, C_2) = T | V$
  1. $\\{T, V\\}$
  2. Skip
  3. $T | V$
  4. $String \\& Int = Nothing$ => do not add
- $lub(String, Int, C_1, C_2) = T \\& V \\& Serializable \\& Comparable\langle T | V\rangle$

> As T = V in the resulting context we may simplify $T | V$ to $T$ or $V$ (denoted as $T =:= V$).

Output:
- $T <: lub(String, Int, C_1, C_2) = T \\& V \\& Serializable \\& Comparable\langle T =:= V\rangle$
- $T :> glb(String, Int, C_1, C_2) = T | V = V$
- $V <: lub(String, Int, C_1, C_2) = T \\& V \\& Serializable \\& Comparable\langle T =:= V\rangle$
- $V :> glb(String, Int, C_1, C_2) = T | V = T$
- $RT = Inv<eq(String, Int, C_1, C_2)> = Inv<T =:= V>$

#### Invariance with multiple generics 2

```Kotlin
fun <T, V> foo(t: Inv<T>, v: Inv<V>) {
    val v = when {
        t is InvString && v is InvString -> Inv("string")
        t is InvInt && v is InvInt -> Inv(1)
        else -> null
    }
    // ...
}
```

> Here the difference is that we have `else -> null` instead of `else -> error("")`.
> Consequently, we have to merge we are not allowed to reduce $T | V$ into $T =:= V$.
> But if value exists, then we know that $T = V$.
> Our hypothesis is that we may always reduce such union based on the fact that `v != null => T = V`.
> We just have to teach the data flow graph to extract such statements.
