# Error-Handling

Some abbreviate notes for now:

-   This is important to get right. We can't possibly legislate out all possible
    runtime errors, especially if we are to deal with real-world deadlines that
    may actually be missed. We want to have a mechanism to handle them gracefully

-   Languages can be used to write reliable software if they have good built-in
    error-handling.

-   Most modern languages, including Go and Rust, use an exception model, where
    panics are basically used as glorified non-local returns. But the restriction
    is that error-handling only happens within the same thread/process, within
    the same callstack.

-   A notable exception is Erlang, which is also known for its reliability. In
    Erlang, processes are _assumed_ to fail all the time (the language doesn't
    even have a static type system, and also supports hot-loading code). Its
    killer feature is that errors are handled via the same mechanism processes
    use to communicate with each other: *message-passing*. Child processes
    can raise exceptions, which are caught by the parent.

-   Can we adopt this error-handling model? We have a strong hierarchical
    relationship between processes, which we already take advantage of to resolve
    data races. But can it do more for us?
