+++
title = "Collecting All the Errors - Rust"
date = 2020-05-09T19:00:00+01:00
images = []
tags = []
categories = []
draft = false
+++

A common way to handle iterating over results, `Result<T, E>`, in Rust is to
use `collect()` to collect an iterator of results into a result.  To borrow the
[example from Rust by Example][collect]:

```rust
let strings = vec!["7", "42", "one"];
let numbers: Result<Vec<_>,_> = string
    .into_iter()
    .map(|s| s.parse::<i32>())
    .collect::<Result<Vec<_>,_>>();
```

You only need either the type hint on `numbers` or the turbofish on `collect`
to coerce the iterator of results into a result.  I prefer the turbofish on
`collect` as I feel it makes the code flow better when reading, and is what you
would do when chaining further computations.  In fact, if you're going to use
`?` on the collected result, you wouldn't _need_[^1] either.

I use this pattern a lot, and it's great. It makes mapping over collections
with operations that can fail as ergonomic as applying that operation to a
single value.

* You can use the `?` operator to make the error handling almost automatic.
* You can use any of the other result combinators like [`and_then`][andthen],
  and [`map`][map].
* You can chain further computations on the end of the expression.  In the
  above example we could have done:

```rust
    .collect()?
    .into_iter()
    .map(|n| n + 1)
    .collect()
```

This pattern basically says:

> Go try these things, if one of them fails we'll fail ourselves,
  reporting the first error that we encounter.

Which, most of the time, is all you need.

But what about when it's not? What about when you want to collect all of the
errors when there are some?

The [very next section of Rust by Example][partition] shows how you can use
`partition(Result::is_ok)` to separate the `Ok` & `Err` variants in your
iterator of results.  You could then go onto write some logic so your function
tests that the collection of errors is empty, and if not returns a result with
a collection of errors in its `Err` variant: `Result<Vec<T>, Vec<E>>`.

But then your entire function stack up to the point where you terminate the
need for expressing a collection of errors has to deal with returning a
collection of errors.  If you're writing a CLI tool, that termination point
would be the point where you write all the error messages to stderr and exit,
so your whole program is likely to need this error handling. This ruins a lot
of the ergonomics of `?` for dealing with calls to foreign methods that return
a normal `Result<T, E>` (`where E: std::err::Error`). Ok, you could replace all
`?` with `.map_err(|e| vec![e])?`, but that's still clunky to deal with.

An alternative could be to use an error type such as
[`valid::ValidationError`][validerror] that can model a collection of errors,
or build your own.  This is probably a better approach, but is still going to
be a bit of work to define your own error type, or integrate another crates'
error type into your application.

If you're building a library, this alternative is probably the right way to go
as your library will be able to return a well-typed expression of "many
errors".

With both of these, you're still going to be missing the ergonomics of
`collect()`.

In much the same way as when building a library you might use
[`thiserror`][thiserror] to define your own error types, whereas in an
application you'd be happy raising add-hoc errors with
[`anyhow::anyhow`][anyhow]: if you're writing an application you might find
defining your own error type(s) to handle "many errors" a bit heavy weight.

I've recently found myself in this exact situation while writing a CLI tool.
The tool needs to tell the user all the errors they've made in the file they've
provided, rather than just one at a time... as that would be super annoying.
It doesn't need to do any real processing of the collected errors, it only
needs to print them all to the terminal in a way that's reasonably readable to
the user, ideally with writing as little special code as possible.

To keep things as simple as possible: I wanted the ergonomics of `collect()`;
and for my representation of "many errors" to be "nothing special", so I can
use all the existing error handling goodies out of the box.

Enter [`BeauCollector`][beau].  Trait `BeauCollector` provides method
`bcollect` which will collect an iterator of errors into an
[`anyhow::Result`][anyhowres] with an `Err` variant containing an ad-hoc
[`anyhow::Error`][anyhowerr] with the messages from each error in the
collection of results on a new line in the error message.

Looking at out example above:

```rust
use beau_collector::BeauCollector as _;

let strings = vec!["7", "42", "one"];
let numbers = string
    .into_iter()
    .map(|s| s.parse::<i32>())
    .bcollect::<Vec<_>>()?;
```

Errors with chains of causes, have their causal chains retained by being
formatted with the [inline representation][inline] where causes are separated
by `:`.

One of the results you're iterating over might have been the result of running
`bcollect`.  As string concatenation is basically what we're doing here,
`bcollect` will represent them alongside any new errors from this layer in the
error message.  You can collect errors with `bcollect` at various layers within
you application and they'll all be represented in one large error at the top
level.

Using `bcollect` at multiple levels and having a `main` that returns a result:

```rust
fn main() -> anyhow::Result<()> {
    use anyhow::Context as _;

    input_file = ...;

    some_data = process_input(input_file)
        .with_context(|| format!("Errors found while processing {}", input_file))?;

    run_application(som_data)
}
```

Can have an error output that looks like:

```
$ ./my-cli-tool input_file.yaml
Errors found while processing input_file.yaml

Caused by:
  "foo_bar" is not a valid name for X: underscores are not permitted

  field "baz" is missing from input data

  errors processing scale data
  :
  54 is too high for field_y
  values must be positive: value -3 given for field_z is negative.

  some other error because of: another error because of: specific problem.
```

The error in the `Err` variant of the result returned by `main` is an
[`anyhow::Error`][anyhowerr] with a message being the block seen below "Caused
by:" and a context of "Errors found while...".

Using [`BeauCollector`][beau] in this way has the advantages that:

* Functions can return a [normal `Result`][anyhowres],
* `?` can be used normally on all other `Results` in functions,
* If `main` returns `Result`, no special error handling is needed and the full
  collection of results will appear in stderr.

There are some limitations too:

* Beyond placing each collected error on a newline, `bcollect` doesn't do any
  further formatting, or reformatting when different layers' errors are
  combined.  It's left up to the error messages raised to add in some
  whitespace to appear more readable than a solid block of text.
* While the messages from the causal chain of an error are retained you loose
  some other error handling like getting the backtrace or downcasting to get
  the original error back.

These are currently the price you pay for the ergonomics of `bcollect`.

For writing applications, you're likely to be happy with this trade off and
find the limitations of `bcollect` a small price to pay.

For a library you might want the caller to have more options on what to do with
the many errors than building up an error message, maybe not.  Perhaps
`bcollect` could be enhanced to return an error type that can represent a tree
of errors that can be added to as errors propagate back up the stack.  By all
means [send in a pull request or raise an issue][github].

[inline]: https://docs.rs/anyhow/1.0.28/anyhow/struct.Error.html#display-representations
[github]: https://github.com/tarquin-the-brave/beau-collector/
[anyhowerr]: https://docs.rs/anyhow/1.0.28/anyhow/struct.Error.html
[anyhowres]: https://docs.rs/anyhow/1.0.28/anyhow/type.Result.html
[beau]: https://docs.rs/beau_collector/0.2.1/beau_collector/index.html
[thiserror]: https://docs.rs/thiserror/1.0.16/thiserror/
[anyhow]: https://docs.rs/anyhow/1.0.28/anyhow/macro.anyhow.html
[validerror]: https://docs.rs/valid/0.3.0/valid/struct.ValidationError.html
[partition]: https://github.com/rust-lang/rust-by-example/blob/master/src/error/iter_result.md#collect-all-valid-values-and-failures-with-partition
[map]: https://doc.rust-lang.org/std/result/enum.Result.html#method.map
[andthen]: https://doc.rust-lang.org/std/result/enum.Result.html#method.and_then
[collect]: https://doc.rust-lang.org/stable/rust-by-example/error/iter_result.html#fail-the-entire-operation-with-collect

[^1]: When I say you don't _need_ the type hints, that is to say that the
      compiler won't need them to infer the type.  HUman readers of the code
      might prefer it is there so they can more easily infer the type.  Type
      hinting is also helpful when refactoring as it can catch errors.
