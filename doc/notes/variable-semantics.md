# Variable Semantics

## Proposal (2021-04-12)

There are primitive *data types*, and then there are *reference types* which
"point to" data types. The particular sorts of data types are not particularly
important here, though they are understood to correspond with "primitive" values
used for computation, such as the integers, booleans, etc.

### Bindings

*Let-bindings*, or just "bindings", bind an identifier to a value, and are not
guaranteed to be allocated storage in the running activation record. The syntax
to bind the value of expression `expr` to `x` is written as follows:

    let x = expr

`x` may be used in subsequent expressions, and its value will be that of `expr`.
A let-binding may be optionally annotated with the type `T` of `expr`:

    let x: T = expr

Note that references may not be taken to of let-bound variables. Let-bound
variables may be shadowed.

(Aesthetic note: I don't really like `=` as the token separating the identifier
and the expression. I think `:=` is a nice alternative, or `<-`.)

### Declarations and references

Bindings contrast *var-declarations*, or just "declarations", which allocate
storage for values in the current activation record. A declaration of variable
`r`, initialized to the value of expression `expr` is written:

    var r = expr

(We defer introducing type annotations declarations until we explain the syntax
for reference types.)

Declarations bind identifiers to references which may be used to access or
modify the underlying value. References need not be explicitly dereferenced to
obtain their value. This means that the following will bind the _value_ of `r`
to `x`:

    let x = r

UPDATE:
The rule is that all reference types are automatically dereferenced one-layer deep.

References are themselves values; to *alias* the reference `r` to `x`, we use
the `&` operator to take the reference value of `r` and bind it to `x`:

    let x = &r

References cannot be taken of let-bound variables.

Reference types are also designated by the `&` type constructor.

    var x = 0
    -- Type of x: &Int

When declaring a `var` with an explicit type annotation, the `&` is automatically
added to the bound variable, so no need to include it:

    var x: Int = 0
    -- Type of x: &Int

Under the hood, there will be a `__ref__: a -> &a` function that takes a value
of type `a`, allocates space for it, stores the value in that space, and returns
the reference to that allocated space. Var-declarations will desugar roughly
as follows:

    var x: T = v
    let x: &T = __ref__(v)


(Note: the more I look at this, the more I don't like the lack of `&` in the
type annotation of our var-declaration syntax. It may be superfluous, but not
having it looks confusing, and it is only one character.)

### Routine parameters and delayed assignment

Routine parameters are *immutable*, and should be treated like local
let-bindings. This eliminates the distinction behind pass-by-value vs
-by-reference semantics, and allows language implementations to choose whichever
is most appropriate.

References may also be passed in by value, and can be used to mutate non-local
variables. For example, the following routine `isEven` receives parameters `n`,
an integer value, and `r`, a boolean reference. It "returns" its result to `r`
by assigning to it (with the `<-` operator):

    isEven (n: Int, r: &Bool) =
      r <- n % 2 == 0

Since reference values are automatically dereferenced, the following will also
typecheck, though the semantics are different---`n` is automatically dereferenced
to obtain the value, which is used for the parity check:

    isEven (n: &Int, r: &Bool) =
      r <- n % 2 == 0

However, since the reference coercion rule only goes one level deep, we need to
explicitly dereference any references stacked further:

    isEven (n: &&Int, r: &Bool) =
      -- r <- n % 2 == 0 -- Typecheck fails
      r <- *n % 2 == 0

The `(<-): &a -> a` operator expects a reference type as its left operand, and
expects a value of that type as its right operand. The level of references must
match up:

    isEven (n: Int, r: &&Bool) =
      -- r <- n % 2 == 0 -- Typecheck fails
      *r <- n % 2 == 0

### Routine return values

(WORK IN PROGRESS)

We want to support syntactic sugar for return values to routines. That is, rather
than explicitly providing the return site, we would like to write the following:

    isEven (n: Int): Bool =
      -- other stuff
      n % 2 == 0

When the last statement is a plain expression, it will be used as a return value.
(TODO: We may also provide a separate `return` keyword to allow control flow to
be terminated earlier.)

The caller should be able to invoke this function as in any other programming
language:

    let r1 = isEven(2)
    -- r1: Bool == 2

    var r2 = isEven(2)
    -- r2: &Bool |-> 2

    var r3 = False
    r3 <- isEven(2)

These should desugar to:

    var _r1 = __POISON__
    fork isEven(2, &_r1)
    let r1 = _r1

    var r2 = __POISON__
    fork isEven(2, &r2)

    var r3 = False -- same as before
    var _t1 = __POISON__
    fork isEven(2, &_t1)
    r3 <- _t1

TODO: The semantics of multiple function calls per-expression are still yet to be
decided on. The most intuitive interpretation would be that all routine calls in
an expression will be forked and called at once.

    -- call site
    let x = isEven(0) && isOdd(0)

    -- Should && be short circuit or not? If not, the following desugaring works:
    var r1, r2 = False, False
    fork isEven(0, &r1), isOdd(0, &r2)
    let x = r1 && r2

However questions remain: what should the fork call order be? This is important
because if they share references and race on them within the same instant, we
need a well-defined call-order to determine priority. Also, how do we handle
short-circuiting?

Variables introduced by let-bindings nor parameters cannot be assigned to using
the `<-` operator.

*Delayed assignments* may be made to references. For instance, the following
`blink` routine alternates between writing 1s and 0s to `led` every 50ms:

    blink (led: &Int) =
      loop
        50ms later led <- 1
        wait led
        50ms later led <- 0
        wait led

Regular assignments are simply a special case of delayed assignments with delay
time 0. So the following two statements are semantically equivalent:

    r <- v
    in 0s, r <- v

    in 10s, led <- 1 -- this one

### Arrays and slices

> FIXME_

For any type `T` and positive (non-zero) integer `n`, we have *constant-size
arrays*, denoted as `[n]T`. We can construct constant-size array values
using array notation, using either let-bindings or var-declarations:

    let a: [3]Int = [1, 2, 3]

    var b: [3]Int = a
    -- b: &[3]Int

Note that a reference to an array is different from a array of references:

    var i = 3
    let x: [3]&Int = [&i, &i, &i]
    x[0] <- 1
    -- i |-> 1

We can retrieve the value stored at some index using array access notation.
For some array `a: [n of T]` and integer `i`, the return type of `a[i]` is `?T`,
where `?` is the type constructor for the optional ("maybe") type. It returns
`Some x` when `i < n` and `None` otherwise.

*Slices* are a reference type that point to some collection of values, and
whose size is known only at runtime. A slice of type `T` is denoted by `[T]`.
Slices can be taken from references of array values using the prefix `[]` operator.
Continuing from the example from before:

    -- b: &[3]Int
    let c: []Int = []b
    c[1] <- 3

Note that, strictly speaking, we must first var-declare an array before we can
obtain a slice to it from its reference. We use the following syntax sugar to do
both (with the optional type annotation):

    let a: [3]Int = [1, 2, 3]

    var c = a
    let c: &[3]Int = ref a

    c[1] <- 2

    var[] c = a
    let c: []Int = ref[] a

    c[2] <- 1

    let c: []Int = ref a

    -- Which one?
    var c = [1, 2, 3]
    -- &[3]Int
    var d = [1, 2, 3]
    -- []Int

Note that in this case, `a` must be an expression whose type is an `Int` array.

Slices are implemented using "fat pointers", which store a length, a pointer to
the underlying struct's `cv_t`, and an index to the first element of the slice.
To check the length of an array, we use the `.len()` method, e.g., `c.len()`.
Like arrays, slices can be indexed using array access notation, and return an
optional type.

Shorter slices may always be taken from longer slices, but once shortened,
a slice cannot grow. To take a slice of all but the first element of `c`, we
may write:

    assert(c.len() == 3)
    let d: [Int] = c[1:]
    assert(d.len() == 2)

Slices that are out of range yield empty slices:

    let e: [Int] = c[42:]
    assert(e.len() == 0)

Note that plain references may be thought of as a special case of slices of
size 1, where the size is known at runtime. One can also retrieve an optional
reference from some index using a reference array access:

    let x: ?Int = c[&0]

Delayed assignments may be made to any item pointed to by a slice. Waiting on
a slice blocks until any of the underlying items is assigned to (disjunctive
semantics). Routines may also block on individual elements or sub-slices.

### Lifetimes

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

### TODO

-   Structs/ADTs
-   Optional types and error-handling
-   Dictionaries
-   Type-classes

## Survey of Languages

The languages of interest below are chosen because they are all compiled systems
languages designed with concurrency, performance, and safety in mind, (aside
from C, discussed here because it is the ancestor of the other languages, and
also because it is widely used in safety-critical concurrent code nonetheless).
Meta-note: it might also be worth writing down some in-depth discussion of why
C is used in these cases.

### C

C uses **pass-by-value**, sticking closely to the capabilities available at the
assembly level/exposed by the ABI. Passing composite values as parameters is
not allowed (though this may be supported as a GCC extension).

In fact, everything in C is a value. To support pass-by-reference, C exposes
the `*` type constructor for pointers; the `&` may be used to take the address
of an lvalue and produce a `*`-typed value. C requires pointer types to be
explicitly dereferenced with the `*` operator; in the special case of
dereferencing a struct pointer to access a field, the `->` operator is given as
syntactic sugar for `*` followed by `.`.

By default, declared variables are mutable. The `const` type specifier may be
used to discourage modification, though it can be cast away due to C's weak type
system.

References:

- Function definitions: <https://en.cppreference.com/w/c/language/function_definition>
- `const`: <https://en.cppreference.com/w/c/language/const>
- lvalues and others: <https://en.cppreference.com/w/c/language/value_category>

### Zig

Zig exposes the same low-level details as C, but provides language features to
help manage the hazards associated with low level memory management.

Zig has three types of pointers: single-item pointers `*T`, slices `[]T`, and
multi-item pointers `[*]T`. Pointer arithmetic is forbidden on single-item
pointers, which are meant to be used only as references. Slices are store both
a pointer to the first element of an array and the length, and are used for
arrays whose length may only be known at runtime. Multi-item pointers appear
rather unsafe, since they designate an unknown number of items. However, both
slices and multi-item pointers, as well as the arrays they point to, support
sentinel-terminated pointers, where a special value is used to designate the
terminal value. For instance, `[*:x]T` designates a multi-item pointer to an
array with elements of type `T` and with a sentinel of value `x: T`. This is
useful for implementing arrays like C's strings and `argv`.

Zig uses **pass-by-value** for simple types, but does not specify whether
composite types will be passed by value or by reference, since these will be
immutable. Addresses taken of parameters should be treated as having a lifetime
that ends when the function returns.

Zig does not rely on an allocator for anything by default, so functions that
require allocator must take one as a parameter by default. The language does not
support any reasoning about lifetimes, and it places the onus on the programmer
to ensure the absence of dangling pointers.

References:

- Pointers: <https://ziglang.org/documentation/master/#Pointers>
- Slices: <https://ziglang.org/documentation/master/#Slices>
- Parameter passing: <https://docs.julialang.org/en/v1/manual/functions/#Argument-Passing-Behavior>
- Memory: <https://ziglang.org/documentation/master/#Memory>
- Lifetimes: <https://ziglang.org/documentation/master/#Lifetime-and-Ownership>
- Undefined behavior: <https://ziglang.org/documentation/master/#Undefined-Behavior>

### Go

Go uses **pass-by-value**, like its ancestor, C. It additionally supports
passing composite types by value.

Like C, Go also supports pointers, though forbids pointer arithmetic (except in
unsafe code). The element access `.` operator automatically dereferences
pointers one level deep, so the `->` operator is not needed, though deeper
levels still require derefencing via the `*` operator. Dereferencing a `nil`
value leads to a run-time error.

Go uses **reference types** for containers of an unbounded size; **slices** for
unbounded-length arrays, and **maps** for partial maps. In particular, slices
may be thought of as an abstraction over C's array pointers, except they are
implemented as "fat" pointers---they contain both the length and a pointer to
the first element. Slices may be copied around using the `copy()` built-in.
The `append()` built-in is used to create a copy and append additional items to
the end of a slice.

Channels and functions are also reference types, since they encapsulate
a pointer/layer of indirection.

Go has the `const` keyword for declaring constant local and global variables,
but it cannot be used for parameters. The address of a const variable cannot be
taken.

References:

- Function calls: <https://golang.org/ref/spec#Calls>
- `.` operator: <https://golang.org/ref/spec#Selectors>
- `&` operator: <https://golang.org/ref/spec#Address_operators>
- Slices: <https://golang.org/ref/spec#Slice_types>
- `append()`: <https://golang.org/ref/spec#Appending_and_copying_slices>
- Channels: <https://golang.org/ref/spec#Channel_types>

### Swift

Swift uses **pass-by-value** for value types---structs, enums, and tuples---but
**pass-by-reference** for reference types---classes. The reference manual (and
various other sources) strongly encourage using value types unless sharing or
inheritance is necessary.

Swift doesn't have first-class pointer types, but emulates passing them to
functions via **copy-in, copy-out** parameters, declared with the `inout`
keyword; at the call site, these parameters must be passed in using the `&`
operator. Their semantics is exactly as described---they should be thought of as
being copied in upon function invocation and copied out upon function return,
rather than as references. Swift imposes additional restrictions, such as not
allowing more than one `inout` parameter to be passed into a function from the
same composite value type.

Swift declarations default to being immutable (like `let` declarations), so
function parameters cannot be modified (unless they are declared as `inout`
parameters).

Swift includes syntax sugar for classes that implement core, type-parametrized
data structures, its **collection types**: variable-sized arrays, sets, and
dictionaries. These wrap generic classes, and are thus reference types.

There are additional considerations when dealing with closures, the details of
which will not be explored here.

References:

- Value and reference types: <https://developer.apple.com/swift/blog/?id=10>
- Functions: <https://docs.swift.org/swift-book/LanguageGuide/Functions.html>
- Inout parameters: <https://docs.swift.org/swift-book/ReferenceManual/Declarations.html#ID545>
- Memory safety: <https://docs.swift.org/swift-book/LanguageGuide/MemorySafety.html>
- Collection types: <https://docs.swift.org/swift-book/LanguageGuide/CollectionTypes.html>
- Closures: <https://docs.swift.org/swift-book/LanguageGuide/Closures.html>

### C++

C++ supports everything that C does, but adds two new paradigms: **reference**
and **move** semantics.

References may be thought of as bindings to "dereferenced pointers". The `&`
type specifier may be used to declare an lvalue reference, and allow functions
to refer to non-local, non-global values without using pointers. The `&&` type
specifier may be used to declare an r-value reference, which extends the
lifetime of an rvalue. The `const` keyword is used to create immutable
references.

References are not first-class values/objects, so cannot be stacked, and only
participate in the type system at the top level (e.g., one cannot have an array
of references).

C++ allows composite data types to be passed by value, reference, or
move---which discipline is used depends on how the parameter is declared. When
passed by value, the copy constructor (default or user-defined) is invoked to
produce a new instance of an object; when passed by move, the move constructor
(default or user-defined) is invoked to transfer ownership into a function. No
constructor is called when passing by reference. To forbid an class's instances
being passed by value or move, one can explicitly delete the copy/move
constructor (preventing synthesis of the default implementation).

C++'s lack of support for outright prohibiting arbitrary side effects in
user-defined constructors mean that copy elision optimisations can observably
affect the semantics of a program, and may lead to memory hazards if copy
constructors are not designed carefully.

References:

- Copy assignment: <https://en.cppreference.com/w/cpp/language/copy_constructor>
- Move assignment: <https://en.cppreference.com/w/cpp/language/move_constructor>
- Copy elision: <https://en.cppreference.com/w/cpp/language/copy_elision>
- References: <https://en.cppreference.com/w/cpp/language/reference>
- Lifetimes: <https://en.cppreference.com/w/cpp/language/lifetime>

### Rust

Like C++, Rust supports pass-by-value, -reference, and -move, but reprioritizes
them. In particular, move semantics are the default, as part of its ownership
type system, while a restricted form of reference semantics are for safe
sharing. Value semantics are supported when explicitly specified.

Rust's move semantics associate exactly one variable with each value at every
point throughout a program's execution; this variable is called the _owner_ of
the value. When a value's owner goes out of scope, the value is _dropped_.
Values representing non-local resources---e.g., pointers to heap data, I/O
handles, etc.---may be cleaned up when they are dropped, by giving an
implementation for the `Drop` trait.

What makes Rust's ownership semantics unique is that it is provided as the
_primary_ mechanism by which values should be bound to variables. While this
implies some amount of copying values around, the lack of aliasing enables
aggressive optimizations that minimize stack space. Note that returning a value
by moving it out of a function coincides with value semantics, since it is
logically copied out of the callee's stack frame into the caller's stack frame.
(I presume aggressive optimizations may eliminate this.)

Rust's references take the place of pointers, but are subject to the same
overarching ownership rules. References are either mutable or immutable
(the syntax preferring the later), available as type constructors `&mut` and
`&`. Ownership may be temporarily transferred from the owning variable to a
reference-typed variable; it is returned when the reference variable is dropped.
The key rule here is that either one mutable reference or multiple immutable
references can exist at any given moment. Note that when a variable is used,
it is typically silently promoted to some kind of reference or another,
preventing mutable aliasing.

Like Go, Rust also has a notion of slices, also used as a generalization of
pointers/references. The difference is that Rust does not allow slices to be
grown.

Rust values and references are parametrized by **lifetimes**, which indicates
how long they will stick around in a program. Rust's borrow checker ensures that
every reference is outlived by the value it refers to, i.e., that it is not
a dangling pointer. These are typically implicitly inferred, but need to be
annotated when the compiler cannot locally and statically determine the
relationship between two references' lifetimes---in particular, for function
parameters and for structs; these need to be annotated explicitly using lifetime
generics. When reasoning about lifetimes, longer lifetimes can always be
"promoted" to a shorter lifetime; the `'static` lifetime refers to the lifetime
of references taken of static/global variables.

Value semantics may be recovered by calling `.clone()` to explicitly duplicate
values, or by implementing the `Copy` trait to allow the compiler to generate
implicit copies.

Something to note is that even at its very "core" (loosely defined), Rust
remains agnostic to how memory should be managed by deferring decisions to the
trait system. For instance, implementing the `Drop` trait for some type tells
the compiler what should happen when an instance of that type is dropped.
Note, though, that programmers cannot implement both the `Copy` and `Drop`
traits for the same type. Behavior over other operators, including the `*`
dereference operator, can be specified via the traits defined under `std::ops`.

Variables are declared immutable by default; use `let mut` to declare a mutable
variable.

References:

- Ownership: <https://doc.rust-lang.org/book/ch04-01-what-is-ownership.html>
- Borrowing: <https://doc.rust-lang.org/book/ch04-02-references-and-borrowing.html>
- Lifetimes: <https://doc.rust-lang.org/book/ch10-03-lifetime-syntax.html>
- Drop trait: <https://doc.rust-lang.org/std/ops/trait.Drop.html>
- Lack of `->`: <https://doc.rust-lang.org/book/ch05-03-method-syntax.html#wheres-the---operator>

### D

D was designed as a successor to C++, and supports pointers as well as garbage
collection.

Interestingly, D allows pointer arithmetic, and its LRM accounts for
dereferencing null and invalid (incorrectly typed) pointers. That is, its
pointers seem to behave similarly to C/C++.

D also supports garbage collection, for data allocated with the `new` keyword.
D used to support an explicit `delete` keyword too, but that has since been
deprecated. All objects do, however, implement the `destroy()` method, which is
used to explicitly invoke the destructor, and optionally reset the object to its
initial state.

D's function parameters use a combination of pass-by-value and -by-reference,
both of which must be explicitly specified. These are designated by parameter
storage classes, which indicate how the parameter is meant to be used (and may
influence the timing of destructor calls). By default (i.e., with no storage
class specified), parameters are passed by value, but there are a number of
options for parameters passed by reference. Unique to D is the `lazy` storage
class, which defers evaluation at the call site to within the callee.

D appears to offer even more built-in memory management options than C++, and
its pointer arithmetic leaves open the possibility of all kinds of memory
hazards. However, it does also come with built-in support for auditing using the
`@safe`, `@trusted`, and `@system` function annotations. `@safe` functions give
rise to "SafeD", a safe subset of D which is less expressive, but where memory
errors are supposedly not possible.

- Pointers: <https://dlang.org/spec/arrays.html#pointers>
- Garbage collection: <https://dlang.org/spec/garbage.html#pointers_and_gc>
- Garbage collection vs pointers: <https://dlang.org/spec/garbage.html#pointers_and_gc>
- `new`: <https://dlang.org/spec/expression.html#NewExpression>
- `destroy()`: <https://dlang.org/phobos/object.html#.destroy>
- Parameter storage classes: <https://dlang.org/spec/function.html#param-storage>
- `inout` parameters: <https://dlang.org/spec/function.html#inout-functions>
- Memory safety: <https://dlang.org/spec/memory-safe-d.html>
- SafeD: <https://dlang.org/articles/safed.html>

### Java

Java uses **pass-by-reference** for all objects, but uses **pass-by-value** for
primitive types (e.g., `int`, `float`, etc.); Java's primitive type wrappers are
immutable (so the distinction between passing by value and by reference doesn't
matter). Its String type is also immutable.

Java relies on its garbage collector to handle cases where the number of
references to an object is unknown until runtime.

Primitive types may be automatically promoted ("autoboxed") to their wrapper
classes.

Value semantics can be recovered using either copy constructors (which needs to
be called explicitly) or the `.clone()` method. However, it's unclear whether
either of these are idiomatic; Java official documentation seems to not mention
copy construction at all (since it is just a specific case of its overloaded
constructors), while `.clone()` is just a method.

Java provides the `final` keyword, which is loosely understood to be like
C/C++'s `const`, except `final` only prevents a variable from being assigned
more than once, but doesn't prevent internal mutability. It doesn't seem like
Java's type system has a notion of immutable shared references.

- Arrays: <https://docs.oracle.com/javase/tutorial/java/nutsandbolts/arrays.html>
- Primitive types: <https://docs.oracle.com/javase/tutorial/java/data/autoboxing.html>

### Kotlin

Kotlin follows the same paradigms as Java, except there are no primitive types;
instead, their wrapper objects (i.e., _primitive classes_) are kept internally
immutable, and intermediate values are left to the garbage collector.

Kotlin makes `final` variables a little more ergonomic by having mutable
declarations be prefixed by `var`, and immutable declarations by `val`.

- `var` vs `val`: <http://kotlin-quick-reference.com/030-R-two-types-variables.html>

### Crystal

Crystal behaves like other object-oriented langauges Java and Kotlin, in that
the default parameter-passing paradigm appears to be **pass-by-reference**, but
provides the abstract superclass `Value` for primitive, struct, and static array
types that cause them to be passed by value.

Crystal also has a pointer type that allows it to interface directly with
memory.

References:

- `Value`: <https://crystal-lang.org/api/1.0.0/Value.html>
- `Pointer`: <https://crystal-lang.org/api/1.0.0/Pointer.html>

### Julia

Julia stands out from the other languages explored here in that it is
a high-performance dynamic language, typically run using a JIT compiler.
Thus it has more in common with "application languages" like Python, Ruby, and
Java, than with systems languages. It correspondingly adopts the
**pass-by-reference** paradigm.

References:

- Argument passing behavior: <https://docs.julialang.org/en/v1/manual/functions/#Argument-Passing-Behavior>

### Haskell

Since Haskell is a resolutely pure language, the difference between
pass-by-value and -by-reference is somewhat moot.

Haskell does have arrays for efficient, constant-time access, but these are
wrappers around unsafe library implementations that are not themselves written
in idiomatic Haskell. For instance, accessing beyond the arrays bounds yields an
undefined value.

Haskell also has mutable references, but again, these are not something that can
be easily and safely implemented in idiomatic Haskell.

References:

- `Array` interface: <https://www.haskell.org/onlinereport/haskell2010/haskellch14.html>
- `Data.Array` module: <http://support.hfm.io/1.1/api/array-0.5.1.0/index.html>
- Mutable references: <https://wiki.haskell.org/Library/ArrayRef>

### OCaml

Like Haskell, OCaml is a functional whose core computational abilities do not
rely on side effects or mutation. However, OCaml also supports special `ref`
types that behave like pointers, for efficient, in-place modification.

In the types namespace, `ref` is a type constructor of kind `* -> *`, and can be
stacked to produce references to other references. In the expression namespace,
`ref` is a function that constructs a reference to the value given as its
argument. The `:=` operator is used to assign a value to a reference, while
the `!` operator is used to dereference a reference and obtain the underlying
value.

OCaml also supports stateful arrays.

Note that one difference from the pointers and references in other languages is
that OCaml does not allow one to refer to existing variables or values.
References can only be obtained by directly allocating them with `ref`, so with
no pointer arithmetic and a garbage collector, references are always guaranteed
to be valid, and no aliasing can take place unless a reference is explicitly
shared.

References:

- Pointers: <https://ocaml.org/learn/tutorials/pointers.html>
- Arrays: <https://ocaml.org/releases/4.07/htmlman/libref/Array.html>

### Nim

Nim procedures' parameters are immutable by default, but can be made to
**pass-by-reference** if declared with the `var` specifier. It can also return
references/lvalues, and seems to use some kind of lifetime analysis to determine
whether it is possible to return a reference to a local variable.

Nim supports both garbage-collected references (traced pointers, `ref`) and
unmanaged pointers (untraced pointers, `ptr`). Garbage-collected references may
be allocated using the `new` keyword. It does not protect against dereferencing
null pointers.

References:

- `var` parameters: <https://nim-lang.org/docs/manual.html#procedures-var-parameters>
- `var` return values: <https://nim-lang.org/docs/manual.html#procedures-var-return-type>
- Pointers: <https://nim-lang.org/docs/manual.html#types-reference-and-pointer-types>

## Discussion

### Parameter-passing

Languages seem to adopt one of the following parameter-passing paradigms:

#### Pass-by-value

In the function body, pass-by-value arguments may be treated as specially
declared local variables. It doesn't really matter if arguments are mutable,
since those mutations by the callee aren't seen by the caller. However, if they
somehow refer to a non-local value, e.g., a pointer to one of the caller's stack
variables, mutations to those references can be felt by the caller as side
effects. As a matter of subjective opinion, passing by value may feel more
intuitive to programmers, since functions called with literal values of
primitive types are representative of the common case, where the value is copied
in.

The downside of pass-by-value, however, is that it implies an implementation
that copies arguments upon function calls, which may often by necessary, and
expensive for composite types, especially when their size is unbounded (i.e.,
dynamic arrays). It also requires the language to provide some other way of
enabling (and restricting) mutation; for instance, providing pointer types.
And with pointer types comes the hazard of dangling pointers/references.

Examples: C, C++, Go

#### Pass-by-reference

A cheap workaround to pass-by-value is to use pass-by-reference, where all
parameters are implicitly pointers to values passed in the caller. While this
is clearly far more efficient than copying around composite values, it leaves
open the question of how pointers to rvalues should be maintained. For instance,
what does it mean to be a pointer to an integer constant value?

Since this strategy is common in object-oriented languages with some kind of
interface/trait/type class system, a common workaround is to specify a special
"value" attribute, for which all instances should be copied by value rather than
passed in by reference. Another strategy is to make the values underlying
"primitive" objects immutable, meaning each operation semantically produces
a new instance, leaving the old instance (and all references to it) intact.

However, thinking of variables as binding stateful objects rather than plain
valuse can be confusing to users, especially since basic examples typically use
primitive values. These are problematic because they are special cases, and are
not indicative of the common case of stateful object binding. Returning
locally-declared objects is also problematic, since without a garbage collector,
these necessarily create dangling references.

Examples: C++, Java, Julia, Kotlin

#### Pass-by-move

To help tame aliasing, some languages have introduced a notion of pass-by-move,
where a variable is logically moved into a function parameter. This paradigm is
typically coupled with some notion of lifetimes, and is probably best
exemplified by Rust's ownership system (see that for a more thorough
description), though C++ also supports this paradigm. Lifetimes may be thought
of as a type system that safeguards against dangling pointers.

The downside of lifetimes is that it is hard to strike a balance between safety,
complexity, and expressivity. Assuming safety is a must, implicit lifetimes may
be somwhat limiting and do not always play well with non-local (heap)
allocation, while explicit lifetimes introduce an additional dimesion of
complexity that can be a barrier to productivity.

Examples: Rust, C++

#### Immutability

More of a strategy then a paradigm, immutability is a powerful restriction
because it causes the semantics of pass-by-value and pass-by-reference to
coincide. It allows parameters to be logically passed by value, but implemented
using pass-by-reference. This is typically exploited by pure functional
languages, but is also present in some lesser-known systems languages.

The downside to immutability is still that it still leaves open the question of
how side effects should be emitted from a function, if at all. Most languages
that take advantage of this strategy still expose some pointer/reference type
through which mutation can take place.

Examples: Zig, Swift, Haskell, OCaml, Crystal, Nim

### Pointer alternatives

C programmers know only of one kind of pointer, which can still essentially be
understood as a fancy long integer, but pointer arithmetic often gives rise to
unsafe code. Based on patterns of how pointers are typically used, languages
have devised several strategies for specializing and encapsulating these use
cases:

#### References

Pointers without pointer arithmetic can be used to represent basic indirection,
without the hazard of pointer arithmetic. In some cases, references are given as
a special type constructor, meaning it is possible to have references of
references, while in other languages, references are simply given as a type
modifier, meaning there can be no references to references.

While older languages require pointers to be explicitly dereferenced, especially
for struct pointers, modern languages trend toward syntax that infer when
a pointer should be dereferenced, with help from the type system.

Examples: C++, Rust, Zig, OCaml (pretty much all languages explored except Haskell)

#### Slices

Slices, otherwise known as "fat pointers", are used as a safe alternative to C's
pointer-as-array-handle paradigm. Slices contain both the length of an array and
a pointer to the first element. Note that the underlying array may contain more
elements than a slice indicates. A slice can typically only be shrunken, and
cannot be grown without duplicating the underlying value.

Examples: Go, Rust, Zig

#### Handles

Some languages, especially those that use garbage collection, use a separate
type to refer to allocated values that should later be garbage collected.
This prevents them from being confused with references to local variables.

Examples: OCaml, D, Nim
