# Slices

## Background

Assume we have a simple, C-like language, with contiguous, fixed-size arrays:

```c
int xs[3];
```

In these cases, size of the array is part of the type, which is useful because
it makes stack allocation and `sizeof` predictable. (We don't want to assume the
availability of a heap.)

At first glance, it would seem that we cannot write size-generic functions. For
instance, consider the function `getLast`, which retrieves the last element of
an `int` array. We can approximate this function by writing a family of
functions for each possible array size:

```c
int getLast1(int (*a)[1]) {
    return (*a)[0];
}
int getLast2(int (*a)[2]) {
    return (*a)[1];
}
int getLast3(int (*a)[3]) {
    return (*a)[2];
}
/* etc. */

// call site:
getLast3(&xs);
```

Assume that, for the purposes of this example, the postfix `[]` operator is just
a primitive over array _values_ (and `a[i]` does not just desugar to `*(a+i)`
like in C).

This scheme is naive, cumbersome, and unnecessary. Since fixed-size arrays are
allocated contiguously, we can refer to the entire array simply by a pointer to
the first element (as is already standard in C). This removes the size of the
array from its type:

```c
int getLastMagic(int *a);
/* implementation omitted */

// call site:
getLastMagic(&xs[0]);
```

However, `getLastMagic`'s type signature discards information that its parameter
`a` is part of an array. It is even possible to call this function using
a pointer to a single `int`. Without providing additional information (or
magic), it isn't possible to infer `a`'s length at runtime.

Note that there are ways to make `getLastMagic` work. For instance, for C
strings, we use a sentinel value `'\0'` to indicate the end of the array.
Meanwhile, in Pascal, we pass around a pointer to an array descriptor (for
strings only), which indicate the size of the underlying array, as well as
containing the payload of the array.

A common workaround in C is to simply supply the length as an additional
parameter:

```c
int getLastLen(int *a, size_t len) {
    return *(a+len-1);
}

// call site:
getLastLen(&xs[0], 3)
```

This is somewhat cumbersome, because it is the programmer's responsibility to
make sure that the given `len` parameter is correct. This function becomes
unsafe if `len` exceeds the actual size of the array that `a` points to.

## Implementing Slices

Slices are a language-provided abstraction over the pattern in `getLastLen`;
they are roughly implemented as follows:

```c
typedef struct {
    const int *p;
    const size_t len;
} int_slice_t;
```

(Assume for now that we are allowed to pass structs by value.) We can implement
`getLast` using slices:

```c
int getLastRaw(int_slice_t a) {
    return *(a.p+a.len-1);
}
```

This function is safe if we have control over all instantiations of
`int_slice_t`, so we can ensure slices are always well-formed (i.e., `len` does
not exceed the length of the underlying array). This can provided as a builtin
that promotes any pointer to an `n`-size array with elements of type `T` to
a slice of type `T`. For instance, given our integer array `int xs[3]` from
earlier, we can promote it to an `int_slice_t` whose value is
`(int_slice_t) { .p = &xs, .len = 3 }`.

The above implementation, `getLastRaw`, should not be what is exposed to the
programmer, since they may still abuse the underlying pointer and ability to
perform pointer arithmetic. Instead, we can provide array accesses as an
operation over slices themselves (denoted by subscript `[]`, implemented as
`operator[]` thanks to C++), and `len` as a builtin function that reads the
`len` field:

```cpp
int operator[](int_slice_t a, size_t i) {
    if (i < a.len)
        return *(a.p+i);
    else
        panic();
}

size_t len(int_slice_t a) {
    return a.len;
}
```

Note that in `operator[]`, we can insert appropriate bounds-checking to panic as
necessary and avoid undefined behavior. Beyond these builtin functions, we do
not allow the user to directly access the `.p` and `.len` fields of slices.
`getLast` may thus be implemented as follows:

```c
int getLast(int_slice_t a) {
    return a[len(a)-1];
}

// call-site:
getLast([]xs); // "slice of" xs
```

## Sub-Slices

Slices appear opaque to the user, meaning they cannot directly see or modify the
values of the fields `.p` or `.len`. However, we can expose additional builtin
methods to allow safe transformations over slices.

We say a slice is "safe" if and only if `.p + i` points to a valid object, for
all `i` from `0` to `.len-1` (inclusive), and assuming that the pointer
arithmetic operatation `.p + i` increments the pointer `.p` by `i*sizeof(*.p)`
_bytes_ (like C-style pointer arithmetic). We can infer three invariants:

-   A slice is vacuously safe if its length is zero.
-   If a slice of `{.p = p, .len = l}` is safe and `l > 0`, then a slice of `{.p
    = p, .len = l-1}` is also safe.
-   If a slice of `{.p = p, .len = l}` is safe and `l > 0`, then a slice of `{.p
    = p+1, .len = l}` is also safe.

Put plainly, subsets of a safe slice are also safe. With these in mind, we can
implement a `subslice` function which truncates a slice according to `start` and
`end` indices:

```c
int_slice_t subslice(int_slice_t a, size_t start, size_t end) {
    if (start >= end || start >= a.len) {
        return (int_slice_t) { .p = NULL, .len = 0 };
    }
    return (int_slice_t) { .p = a.p + start, .len = end - start };
}
```

This is typically written using the notation `a[start:end]`, where `a[:end]` is
syntactic sugar for `a[0:end]`, and `a[start:]` is sugar for `a[start:len(a)]`.
The subslicing operation allows us to shrink a slice.

## Return values

TODO.

## Alternatives

TODO.

## Discussion

Stephen's proposed variant: counted arrays/strings. Every array, including array
literals, starts with a size_t count of its length, followed by the payload:

```c
struct int3 {
    size_t len = 3;
    int payload[3];
};
```

For any size, we can have a struct like this. At compile time, we need to know
the size of any array we're allocating. So we might end up just generating a new
type for every possible compile-time size.

Introduce one more type: `struct intN`:

```c
struct intN { // abstract class, never instantiated, used as a pointer to
    size_t len;
    int payload[1];
}
```


```
Slice = {ptr: &[_]Int, start: Int, len: Int}
```

When you refer to any array, you give it the type `struct intN *`. These can be
passed an returned like any value. However, when you are working with one of
these, you can look at the `.len` field. You can safely cast a `struct intN *`
to an `struct int3 *`.

```c
struct int3 a3;
struct intN *p = &a3;
// p->payload[2] is fine in C, because it doesn't do bounds checking.
```

The key difference between this and slices is that the length is part of the
array value. It solves the returning array problem. The main issue is that one
cannot have multiple distinct pointers to the same array, i.e., we cannot do
sub-slices. It saves us a pointer. Another thing this doesn't do is it doesn't
support dynamic arrays, but neither do slices.

A dynamic variant of this adds a capacity field to the sized array, that allows
us to grow it at runtime (up to the capacity).

## Use cases

Slices can be easily used for quick/mergesort, which rely on some notion of
a sub-slice.

How does this play with schedulable?
