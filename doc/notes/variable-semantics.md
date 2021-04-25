# Variable Semantics

## Proposal (2021-04-12)

There are primitive *data types*, and then there are *reference types* which
"point to" data types. The particular sorts of data types are not particularly
important here, though they are understood to correspond with "primitive" values
used for computation, such as the integers, booleans, etc.

### Bindings

*Let-bindings*, or just "bindings", bind an identifier to a pure value.
The syntax to bind the value of expression `expr` to `x` is written as follows:

```
let x = expr
```

`x` may be used in subsequent expressions, and its value will be that of `expr`.
A let-binding may be optionally annotated with the type `T` of `expr`:

```
let x: T = expr
```

Note that references may not be taken to of let-bound variables, since these are
not guaranteed to be allocated storage in the running activation record.
Let-bound variables may be shadowed.

(Aesthetic note: I don't really like `=` as the token separating the identifier
and the expression. I think `:=` is a nice alternative, or `<-`.)

#### Declarations and references

Let-bindings contrast *var-declarations*, or just "declarations", which allocate
storage for values in the current activation record (though a smart optimizer
may eliminate these allocations). A declaration of variable `r`, initialized to
the value of expression `expr` is written:

```
var x = expr
```

The variable `r` is bound to a *stored* value. Stored values are values for which
references may be taken, though they differ from references because they do
not need to be explicitly dereferenced to access the underlying value.
For instance, the following binds the _value_ of `r` to `x`:

```
var x = 3
let y = x
-- y: Int == 3
```

Explicit *references* may be taken of stored values using the `&` operator:

```
var x = 3
let r = &x
-- r: &Int ==> 3
```

The resulting reference type is designated by the `&` type constructor (distinct
from the `&` operator). References cannot be taken of let-bound variables.
The `r: T ==> v` in the comment is our meta-notation to say that the reference
`r` of type `T` points to storage containing the value `v`.

(In our language, references just pointers, so maybe we should just call them pointers?
There is a bit of a confusing analogy to be made with C++; SSM references are
C++ pointers, which SSM stored values are C++ references.)

Reference types are a first-class member in the type system, and are denoted by
the type constructor `&`. (This contrasts stored values, which are not type
constructors but modifiers.) To obtain the value underlying a reference, the
programmer must explicitly dereference a reference value using the prefix
`(*): &a -> a` operator:

```
var x = 3
let r = &x
let y = *r
-- y: Int ==> 3
```

It may be simpler to think of var-declarations in terms of syntactic desugaring,
where they are just used as "dereferenced references". The desugaring rule is,
when a variable `x` is var-declared, we are given an invisible reference `__x` to
its underlying storage. All occurrences of `x` are replaced with `*__x`,
unless prefixed by a `&`, in which case they are just replaced by `__x`:

```
var x = 3
let y = x
let r = &x
-- desugars to
let __x: &Int = __alloca__<Int>(1)
let y = *__x
let r = __x
```

The internal `__alloca__<T>(v)` function allocates storage for type `T` on
the activation record, initialized to value `v` passed as a parameter.

#### Assignment

The advantage of var-declaration is that they support mutation. Given a reference
as the left operand, the `<-` statement updates the underlying storage with the
value given in the right operand:

```
var x = 3
let r = &r
r <- 1
-- r: &Int ==> 1
```

The left operand of `<-` must either be a reference (as shown above) or
a var-declared stored value. That is, the following assignment is also valid:

```
var x = 3
x <- 1
-- &x: &Int ==> 1

let _x: &Int = __alloca__(3)
_x <- *_x + 1
```

*Delayed assignments* may also be made to references. These assignments are
prefixed with a *time qualifier* beginning with the `in` keyword and ending
with a `,`:

```
var x = 3
in 10s, x <- 1
-- x keeps its value in the current instant,
-- but takes the value 1 in the instant 10s later
```

Note that while the assignment takes place 10s later, the destination and source
value are taken from the current instant. For instance:

```
var x = 1
var y = &x
var z = 2
in 10s, *y <- z
-- x (current value of y) is still assigned the value 2 (current value of z)
-- in 10s, even if y and z are modified later on in the same instant:
z <- 3
y <- &z
```

Note that regular assignments are simply a special case of delayed assignments
with delay time 0. So the following two statements are semantically equivalent:

```
r <- v
in 0s, r <- v
```

#### Routine parameters

Routine parameters are *immutable*, and should be treated like local
let-bindings. Note that this language design choice eliminates the distinction
behind pass-by-value vs -by-reference semantics, and allows language
implementations to choose whichever is most appropriate.

References may also be passed in by value, and can be used to mutate non-local
variables. For example, the following routine `isEven` receives parameters `n`,
an integer value, and `r`, a boolean reference. It "returns" its result to `r`
by (instantly) assigning to it (with the `<-` operator):

```
isEven (n: Int, r: &Bool) =
  r <- n % 2 == 0
```

This routine can be invoked as follows, using the `fork` primitive:

```
var b = False
fork isEven(3, &b)
-- &b: &Bool ==> 3
```

The caller must allocate space for the return value, and pass it by reference.
Note, though, that reference-typed parameters must still be explicitly deferenced
to obtain their value. Consider the following variant of `isEven`, which takes
`n` by reference:

```
isEven (n: &Int, r: &Bool) =
  -- r <- n % 2 == 0 -- typecheck fails
  r <- *n % 2 == 0
```

To avoid this and treat it like a var-declared variable, we can prefix the
parameter declaration with `var`. In this case, the `&` type constructor should
be dropped, and the parameter is treated as a stored value in all appearances
(alternatively, following the same desugaring rules described earlier). For
instance, the following function also sets its first parameter to zero after
measuring its parity, all without explicitly dereferencing it:

```
isEven (var n: Int, r: &Bool) =
  r <- n % 2 == 0
  n <- 0
-- desugars into
isEven (_n: &Int, r: &Bool) =
  r <- *_n % 2 == 0
  _n <- 0
```

#### More about stored values

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

#### Routine return values (TODO)

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

### Arrays

For any type `T` and positive (non-zero) integer `n`, we have *constant-size
arrays*, denoted by `[n]T`. We can construct constant-size array values
using array notation, using either let-bindings or var-declarations:

```
let a = [1, 2, 3]
-- a: [3]Int
-- a[0]: Int
```

We can access elements of an array using postfix array index notation:

```
-- continues from before
let x = a[0]
-- x: Int = 1
```

Array values are passed around and assigned by value:

```
-- continues from before
let b = a
-- b: [3]T == [1, 2, 3]
```

Just like any other value, we can pass around references to arrays:

```
-- continues from before
var c: [3]Int = a
-- &c: &[3]Int ==> [1, 2, 3]
```

Note that the above makes a _copy_ of `a`'s _value_, and uses that to initialize
the storage underlying variable `c`. As with any other value let-binding, we
cannot modify `a` (aside from re-binding the name):

```
a <- [1, 2, 3] -- Error: cannot assign to let-bound value a: [3]Int
```

On the other hand, since `c` is var-declared, we can assign to it by array-value:

```
c <- [4, 5, 6]
-- &c: &[3]Int ==> [4, 5, 6]
```

Like with arrays, var-declared arrays are desugared into references, and
automatically dereferenced when used in an r-valued context:

```
var c: [3]Int = a
-- desugars into
let __c: &[3]Int = __alloca__<[3]Int>([1, 2, 3])

c <- [3, 4, 5]
-- desugars into
__c <- [3, 4, 5]

c <- c
-- desugars into
__c <- *__c
```

#### Array indexing

Array elements can be retrieved by index, using array index notation:

```
let a = [1, 2, 3]
let x = a[1]
-- x: Int = 2
```

During var-declared array desugaring, the dereference takes place before array
indexing:

```
var c = [1, 2, 3]

let x = c[1]
-- desugars into
let x = (*__c)[1]
-- x: Int == 2
```
