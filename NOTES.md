# Subtyping reconstruction aka GADT-style inference

Here included some notes and corner cases on the subtyping reconstruction, which is not included in the KEEP.

## Clarification on type variables generated for projection of real types

Some examples showing that type variables generated for projection of real types are unique for different positions despite any local relation between these positions. 

```kotlin
interface Out<out T>
interface OutString : Out<String>
interface OutSerializable : Out<Serializable>

fun <T> foo(a: Out<T>, b: Out<T>) {
    if (a is OutString && b is OutSerializable) {
        // Input: Out<T> & OutString, Out<T> & OutSerializable
        // Constraints: String =:= R1, R1 <: T, Serializable =:= R2, R2 <: T
        // Output: Serializable <: T (and useless String <: T)
    }
}
```

Here it is shown that real parameters for value `a` and `b` are different, and we make transitive closure on them independently.

```kotlin
interface Out2<out T, out V>
interface OutString1<out V> : Out2<String, V>
interface OutSerializable2<out T> : Out2<T, Serializable>

fun <T> foo(a: Out2<T, T>) {
    if (a is OutString1<*> && a is OutSerializable2<*>) {
        // Input: Out2<T, T> & OutString1<*> & OutSerializable2<*>
        // Constraints: String =:= R1, R1 <: T, Serializable =:= R2, R2 <: T
        // Output: Serializable <: T (and useless String <: T)
    }
}
```

Here we also have two different type variables for different positions, because even if they are equal in the current function, it does not mean that they were equal during the creation of this value.

## Some basic tests

Some basic tests could be found in tests for [prototype implementation](https://github.com/e2e4b6b7/kotlin/pull/3) in `compiler/fir/analysis-tests/testData/resolve/inference/gadtInference`.

## Corner case with single value

```kotlin
interface Out2<out T, out V>
interface OutString1<out V> : Out2<String, V>

fun <T> foo(b: Out2<T, T>) {
  if (b is OutString1<*>) {
    // Here T :> String both globally and locally to value b
    // T =:= String is unsound even locally for value b
  }
}

// because of this case:
class MyOut2 : OutString1<Serializable>
foo<Serializable>(MyOut2())
```

## Typed AVL tree

[Here](https://github.com/e2e4b6b7/AVLTyped) is implemented a typed AVL tree in Kotlin with GADT-style inference.
It __should__ work after the KEEP is implemented.
It may not work out of the box as it utilizes some indirect equalities with captured types and `is` checks on classes with gadt-style inferred parameters.

To cover more complex cases with this example:
- All `is` checks may be converted into checks with bare types.
- Functions `rotateRight2`, `rotateRight3`, `rotateLeft2` and `rotateLeft3` may be inlined to make equalities with captured types more complex.
