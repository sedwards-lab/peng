# Ongoing Discussions

It gets messy to embed these all in the variable semantics proposal document, so
I separated them out here.

## Top-level TODOs

-   Structs/ADTs
-   Optional types and error-handling
-   Dictionaries
-   Type-classes
-   Motivating example: Conway's Game of Life

## Bindings

### More about stored values

In which I list some examples about var-declared identifiers, and attempt to
formalize them.

We have already seen that this works, and desugars as follows:

```
var x = 3
let y = x
let r = &x
-- desugars to
let __x: &Int = __alloca__<Int>(1)
let y = *__x
let r = __x
```

There isn't coercion going on here so much as it is truly syntactic sugar.
The following will _not_ work, because the stored value `x` is already
dereferenced for us:

```
var x = 1
x <- *x + 1 -- cannot dereference a value of type Int
```

Meanwhile, those desugaring rules do not apply to let-bindings, meaning the following
also fails to work because `y` is not automatically dereferenced:

```
var x = 1
let y: &Int = &x
y <- y + 1 -- type error: in expression y, expected type: Int, got type: &Int
```

Formally, _without_ thinking about these semantics in terms of desugaring, we
may define a type _modifier_ `!` (or as a type constructor that doesn't stack)
that distinguishes plain values of type `a` from stored values of type `!a`.
There is always an implicit coercion `!a >-> a` so that stored values may be
used wherever plain values may be used. We may thus define the reference
operator `&` as a function in terms of the `!` type modifier, such that `(&): !a
-> &a`.

Furthermore, we can think of `<-` as an _overloaded_ operator to support
assignment to both references and stored values. That is, its type is
`(<-): (&a|!a) -> a -> ()`, where `(&a|!a)` represents the intersection type of
references and stored values. (Note that the unitary return type `()` here is
a placeholder for what should be some kind of state monad.)


## Routines

### Return values

We want to support syntactic sugar for return values to routines. That is, rather
than explicitly providing the return site, we would like to write the following:

```
isEven (n: Int): Bool =
  -- other stuff
  n % 2 == 0
```

When the last statement is a plain expression, it will be used as a return value.
(TODO: We may also provide a separate `return` keyword to allow control flow to
be terminated earlier.)

The caller should be able to invoke this function as in any other programming
language:

```
let r1 = isEven(2)
-- r1: Bool == 2

var r2 = isEven(2)
-- r2: &Bool |-> 2

var r3 = False
r3 <- isEven(2)
```

These should desugar to:

```
var _r1 = __POISON__
fork isEven(2, &_r1)
let r1 = _r1

var r2 = __POISON__
fork isEven(2, &r2)

var r3 = False -- same as before
var _t1 = __POISON__
fork isEven(2, &_t1)
r3 <- _t1
```

TODO: The semantics of multiple function calls per-expression are still yet to be
decided on. The most intuitive interpretation would be that all routine calls in
an expression will be forked and called at once.

```
-- call site
let x = isEven(0) && isOdd(0)

-- Should && be short circuit or not? If not, the following desugaring works:
var r1, r2 = False, False
fork isEven(0, &r1), isOdd(0, &r2)
let x = r1 && r2

-- Shortcircuiting desugar
let x = match isEven(0):
        case True -> isOdd(0)
        case False -> False
```

However questions remain: what should the fork call order be? This is important
because if they share references and race on them within the same instant, we
need a well-defined call-order to determine priority. Also, how do we handle
short-circuiting?

## Arrays

### Return type of array indexing

We want to support array index notation on array values, references to arrays,
and slices (which point to underlying arrays).
How can we justify overloading
array indexing on all three?
That is, we also want the following to work just the same:

```
let a = [1, 2, 3]
var v = [1, 2, 3]
let r = &v
let s = []b

let ae = a[0]
-- a: Int == 1
let ve = v[0]
-- ve: Int == 1
let re = r[0]
-- re: Int == 1
let se = s[0]
-- se: Int == 1
```

Semantically speaking, this should be no problem, because there is no ambiguity
here. But it is a question of how we can justify this to the user, preferably
without a complicated web of coercions. Note that Go seems to get away with
this. Given `a: *[n]T`, `a[x]` is shorthand for `(*a)[T]`.

It may also be worth considering: what should the return value of an array index
be, with respect to safety? Right now, array indexing is an unsafe operation;
there is no specification for what should happen upon an index OOB access. This
discussion is also dependent on what error handling and propagation features we
expose to the programmer.

### Assignability of array indexing

We need to clarify the assignability of the expression `a[i]`. Later, we will
need to justify how it should desugar in an l-value context. Consider the
following: an array value, a var-declared array stored value, an array
reference, and a slice. Only the latter three should be assignable:

```
let a = [1, 2, 3]
var v = [1, 2, 3]
let r = &v
let s = []b

a[0] <- 0 -- not ok: cannot assign to let-bound value

v[0] <- 0 -- ok

r[0] <- 0 -- ok

s[0] <- 0 -- ok
```

A tentative suggestion for justifying why `v` and `r` are assignable is to say
that if some `a` is assignable, then `a[i]` should be directly assignable (`a[i]
<- v`). This has two shortcomings:

-   It does not explain why indices slices are assignable. Slices themselves
    should probably not directly assignable---it seems problematic to have to
    deal with assigning a value whose size is only runtime known. Yet we should
    be able to assign directly to the index of a slice. So this seems to violate
    this rule.

-   It leaves open the question of whether `a[i]` should be indirectly
    assignable, i.e.,

    ``` 
    let ai = a[i]
    ai <- v
    ```

    Under a classical notion of let-bindings, we should be able to substitute
    `a[i]` at `ai` in the LHS of `<-`, and thus we should be able to assign to
    it. But then what is the type of `ai`? And is it something we can assign to?

Note that `r[i]` doesn't work when `r` is a reference to an array:

```
var a = [1, 2, 3]
let r = &a
let x = r[0]        -- type error: cannot index into r: &[3]Int
let x = (*r)[0]     -- ok
```

But it is not clear how this should work when the reference appears on the left
side of `<-`:

```
r[0] <- 1           -- type error?
(*r)[0] <- 3        -- suggests we need to have expressions on the left of <-?
```

We currently disagree as to whether the following should be allowed:

```
var i = 0
let a: [1]&Int = [&i]
a[0] <- 3 -- interpretation: the reference obtained from a[0] takes the value 3
```

My discomfort with this to do with its ramifications on how we desugar
assignments to array indices. Allowing this would code implies that the LHS of
`<-` can take any expression, which in turn implies that expressions, at least
some of them, are assignable.

It also makes the following very similar-looking code produce very different
outcomes:

```
var i = 0

let a = [&i, &i]
a[0] <- 1           -- ok; this is the proposal being considered

var b = [&i, &i]
b[0] <- 1           -- not ok; when on the LHS, b is treated as a mutable array
                    -- whose 0th index should be given the value 1 (type error)
-- desugars to
let __b: &[2]&Int = __alloca__<[2]&Int>([&i, &i])
setArray(*__b, 0, 1) -- type error: 1 is not of type &Int
```

This seems to violate the intuition/rule that a var-declared identifier should
be usable anywhere a let-bound identifier is.

### Desugaring array indexing in l-value

How should we desugar `a[i]` in an l-value context? There are three options:

#### `a[i]` as an l-value

`a[i]` is an _expression_ that returns something different in an l-value
context, something that can be assigned to:

```
a[i] <- v
-- is an instance of
<l-value-expr> <- <r-value-expr>
```

With respect to where we are now, this relaxes the rules of what we are allowed
to assign to. This is the strategy employed by languages with mutable variables
like C/C++ (lvalues), Rust (place expressions), and Go (assignable expressions).

The advantage of this approach is that it allows us to use expressions in
l-valued contexts, but has the disadvantage of complicating expressions. In
particular, it forces us to designate an additional qualifier to the type of
every expression, i.e., l-value or r-value.

#### `a[i] <-` as a setter method

`a[i]` on the left of a `<-` desugars to a setter method which mutates the
underlying object `a`:

```
a[i] <- v
-- desugars to
__setArray__(&a, i, v) -- mutates a with a[i] set to v
```

With respect to where we are now, this adds to the desugaring rules, and imbues
variables with a notion of a setter method. This is the strategy employed by
object-oriented languages like Python, Kotlin, and, intriguingly, OCaml. What
these languages seem to have in common is that there is no distinction between
an array value and a mutable reference to one; arrays are inherently mutable.

The advantage of this approach is that it's relatively simple and inherently
efficient, but seems to introduce what seems introduce a notion of mutability
that seems ad hoc, in the sense that primitive and array type assignment cannot
be expressed in terms of one another, and are entirely different
implementations. In other words, it attaches special meaning to "references to
arrays" that cannot be understood by simply composing the meaning of references
and the meaning of array values.

It also means that the following are functionally equivalent, but expressible in
two entirely different ways that do not seem to have anything to do with one
another:

```
var a = [1, 2, 3]

a[1] <- 5
-- desugars to
__setArray__(&a, i, v)

a <- [a[0], 5, a[2]]
```

It leaves open the question of how assignability can distributed to other
processes via reference-typed routine parameters. In particular, how can we pass
`a[i]` to another routine such that it can be assigned? Without this, we cannot
define how we should implement nested array assignments:

```
var aa = [[1, 2]]
__setArray__(a[0], 1, 3) -- doesn't work since a[0] is just a value
```

#### `[i] <-` as a lens

`a[i]` on the left of `<-` desugars to a lens, to which the right operand is
applied:

```
a[i] <- v
-- desugars to
a <- __updateArrayIndex__(a, i, v) -- makes copy of a with a[i] set to v
```

With respect to where we are now, this greatly complicates the desugaring rules,
and necessitates some kind of value transformer for composite types (kind of
like record update notation). This isn't really used by default in any language
(except, in some loose sense, Swift?), but is inspired by Haskell's lens
library, created to express the composability of updating deeply nested members
in a pure context.

The advantage of this approach is mainly that it maintains the simplicity of
assignment, leaving the heavy lifting of computing the updated value to the
expression world, and simply assigning back the new value. However, the
disadvantage is that this is inherently inefficient, and is not how it should be
implemented under the hood anyway.

### Partitioning an array

Can we take some kind of reference to an array, and hand off references to only
sub-slices of that array? For instance:

```
var a = [1, 2, 3]
let a1 = a[:1]
let a2 = a[1:]
let i0 = &a[i]
```

This can be useful for allowing library routines to modify state on a process's
behalf, while restricting external routines' access to the array. For instance:

```
var a = [...] -- some big array
fork libfunc(&a[:a.len()/2]) libfunc(&a[a.len()/2:])
```

The `fork` call spawns two processes executing the `libfunc` on each half of the
array `a`.

We should also clarify the blocking behavior of those references. For instance,
if the first `libfunc` process waits on the first half of `a`, and the second
`libfunc` process modifies some element in the second half of `a`, should the
first process unblock, to find its slice untouched?

A solution to this question is to say that statements can wait/await on
sub-slices, or even individual elements, of a slice. Like waiting on multiple
distinct references, blocking on the multiple elements of a slice will block
until any of the underlying elements is assigned to (i.e., disjunctive
semantics.) And if a slice is empty, perhaps the statement should unblock
immediately (in the same instant).

### Blocking and arrays

Here I describe some examples of different uses of references to arrays vs
arrays of references, both of which can be used to implement interesting
blocking patterns. Each of the following examples can be later generalized to
arbitrarily-sized arrays using slices.

#### References to arrays

This allows us to block on an array when it changes. This is useful for building
routines with merging functionality, such as this routine that waits on incoming
integers written to `a`, and forwards them to output channel `o`:

```
mergeSum3(var a: [3]Int, o: &Int) =
    loop
        a <- [0, 0, 0] -- initialize to 0s
        wait &a
        o <- a[0] + a[1] + a[2]
```

This could be easily made size-generic by converting `a` to a slice (though it
does look uglier):

```
mergeSum(var a: []Int, o: &Int) = 
    for elem in a
        elem <- 0
    loop
        wait &a
        var s = 0
        for elem in a
            s <- s + elem
            elem <- 0
        o <- s
```

#### Arrays of references

This allows multiple references to be passed around with a single name. An
example for where this may be useful is to build dispatchers, routines which
send data to an arbitrary number of consumers, without requiring that the
references be all part of the same array. For instance:

``` 
demux5(var s: Int, var i: Int, os: [5]&Int) =
    loop
        let o = os[s % 5]
        o <- i
        wait [&s, &i]
```

This could be easily made size-generic by converting `os` to a slice:

```
demux(var s: Int, var i: Int, os: []&Int) =
    loop
        let o = os[s % os.len()]
        o <- i
        wait [&s, &i]
```

## Slices

Wishlist for slices:

-   Make it easy to write length-polymorphic functions that operate on arrays.
    This is what Go (and Rust and Zig) use, which have a notion of compile-time
    sized arrays, with slices as an alternative to refer to those arrays. This
    is because a slice is just like an array type constructor, but without an
    explicit size.

-   Managing/enforcing non-interference. If we pass one slice to one process and
    another slice to another, they should be able to run in parallel.

Underneath the hood, slices are roughly represented as follows:

```
data []T = []T {
    ref: *T         -- raw pointer, with which we can do pointer arithmetic
    start: size_t
    len: size_t
}
```

With slices, we can easily build dynamic, size-polymorphic list routines,
without any kind of type-level arithmetic business:

```
-- size-generic function to retrieve last element of an array via a slice:
getLast(a: []Int, r: &Int) =
    r <- a[a.len() - 1]

-- size-generic function to compute mean of array via a slice:
getMean(a: []Int, r: &Int) =
    var m = 0
    for i in a
        m <- m + i
    r <- m / a.len()
```

The size of array types must be known at compile time, which can be rather
cumbersome for writing generic, array manipulating code. Our solution is to use
*slices*, which are a reference type that point to some collection of values, and
whose size is known only at runtime. A slice of type `T` is denoted by `[]T`,
and the length of some slice `s: []T` can be obtained from the expression `s.len()`.

Slices can be taken from references of arrays or stored array values, using the
overloaded prefix `[]` operator:

```
var a: [3]Int = [1, 2, 3]
let b: &[3]Int = &a

let sb = []b
-- sb: []Int ==> [1, 2, 3]

b <- [4, 5, 6]

let sa = []a
-- sa: []Int ==> [4, 5, 6]
```

In the case of a stored array value, the `[]` prevents desugaring from inserting
the implicit dereference in front of the identifier. Also note that while one
can directly assign array values to references to arrays, one cannot do the same
for slices.

Slices are implemented using "fat pointers", which store a length, a pointer to
the underlying struct's `cv_t`, and an index to the first element of the slice.
Like array values, slices can be indexed using array access notation.

Shorter slices may always be taken from longer slices, but once shortened,
a slice cannot grow. To take a slice of all but the first element of `c`, we
may write:

```
-- c.len() == 3
let d: []Int = c[1:]
-- d.len() == 2
-- d ==> [c[1], c[2]]
```

Slices that are out of range yield empty slices:

```
let e: []Int = c[42:]
-- e.len() == 0
```

### Slice declaration

Do we need shorthand for declaring array storage and taking a slice for it?
That is:

```
var[] s = [1, 2, 3]
-- desugars to
var _s: [3]Int = [1, 2, 3]
let s = []*_s
```

Then again, it's not that cumbersome to put a `[]` everywhere we need to use slice
to a stored array value, i.e., when passing by reference to functions and when
assigning to a reference to a slice. In both cases, it seems worth explicitly
noting that we are taking the slice of a stored array value.

## Lifetimes

*Lifetimes* indicate "how long" the storage underlying a reference will live.
Var-declarations produce storage whose lifetime is tied to the lexical scope of
the variable. When the variable goes out of scope, it is said to be *dropped*,
and the underlying storage on the activation record may be made available for
allocating other data.

Lifetimes are totally ordered at runtime. We say `a <= b` if and only if values
of lifetime `a` are dropped before or at the same time as values of lifetime
`b`. We may not know the runtime order of lifetimes at compile-time, so they
appear in our type system as a partially-ordered set, where the relation `<=` is
reflexive and transitive. A greater lifetime can always be coerced to a smaller
lifetime.

The central tenet of our lifetime system is that *references should never
outlive the data they point to*. Reference types are parametrized by the
lifetime of the data they point to; a reference parametrized by lifetime `a`
may only point to values of lifetime `b` where `a <= b`.

TODO: formalize based on Oxide, as discussed here:
<https://aaronweiss.us/pubs/draft20-oxide.pdf>.
