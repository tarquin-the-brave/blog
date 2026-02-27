+++
title = "There's No Such Thing as an Error in Rust"
date = 2026-01-08T20:08:21Z
images = []
tags = ["rust"]
categories = []
draft = false
aliases = ["/posts/no_errors_in_rust/"]
+++

Colleagues have heard me say this.  Usually when we're debugging something, or
discussing error handling approaches, I'll pipe up with:

> "Well, there's no such thing as an error in Rust."

It tends to get a raised eyebrow or two.  So I'll explain.

---

## How Rust Handles Errors

If you're not familiar with Rust, here's the quick version: Rust doesn't have
exceptions.  Instead, operations that can fail return a `Result` type.

```rust
enum Result<T, E> {
    Ok(T),
    Err(E),
}
```

When you call a function that might fail, you get back either `Ok` containing
your value, or `Err` containing some error information.  You then have to
handle both cases.

```rust
fn read_config() -> Result<Config, ConfigError> {
    // ...
}

fn main() {
    match read_config() {
        Ok(config) => println!("Got config: {:?}", config),
        Err(e) => println!("Failed to read config: {:?}", e),
    }
}
```

The `Result` type is what's called an "algebraic data type".  It's a type
that can be one of several variants, each potentially carrying different data.

Rust calls these `enum`s which is a bit annoying as they're not "enums", they're
sum types that contain data.  But I guess it's too late for that to change...

---

## Errors Are Just Data

To Rust, there's nothing special about that `Err` variant.
It's just data.  The compiler doesn't treat it differently.  There's no hidden
machinery.  It's the same as any other value you might pass around your
program.

Contrast this with Python.  When the code encounters data it doens't expect or
know how to handle an exception is raised, and then the code execution and data
flow disappears from the page and is maybe picked up somewhere else in the 
codebase, up the call stack, if that type of exception is `except`'d'.  It's 
like the data gets thrown into this hidden subway running underneath the code.  
There's nothing explicit in the code that might raise the exception whether it
would, or what it might raise.

And when you're writing code, you might forget to catch the right exception.
Or any exception at all.  By default, exceptions propagate silently up the
call stack until they hit something that catches them - or they don't, and
your program crashes.  You're not being made to handle error data, you have to
opt into doing it.

In Rust, everything is explicit.  The function signature tells you it returns
a `Result`.  You have to acknowledge the possibility of failure to get at the
success value.  You're exposed to the full complexity of what you're doing,
always.  Even when you don't really care about the details and it's
excruciatingly painful to do so...

---

## Result Is Just a Convention

The `Result` type is arbitrary.  It's just
a convention that the standard library and ecosystem have agreed upon.  Other
than the `?` operator (which I'll get to), there's nothing special about it.

You could define your own:

```rust
enum Either<L, R> {
    Left(L),
    Right(R),
}
```

And decide that `Left` is where you put your error information.  The compiler
wouldn't care.  Your code would work just fine.  You'd lose the `?` operator
and interop with the standard library, but structurally there's nothing
magical about `Result`.

The `?` operator is syntactic sugar that makes working with `Result` more
ergonomic:

```rust
fn do_stuff() -> Result<Value, Error> {
    let config = read_config()?;  // Returns early if Err
    let data = fetch_data(&config)?;
    Ok(process(data))
}
```

But even `?` is just desugaring to a match expression.  It's not some deep
language primitive for error handling.  Rust doesn't have a concept of
"errors" built in.  It has sum types, and one of them is conventionally used
for representing fallible operations.

---

## Errors Are Just Data

I keep coming back to this phrase: "errors are just data".  And here's the
philosophical bit.

It's not just in Rust that errors are just data.  To computers, *errors are
just data*.  Always have been.  The whole concept of an "error" is a human one
that we've imposed over patterns of bits.  The computer just sees ones and
zeros.  It doesn't know that this particular combination means "file not
found" and should make the human sad.  We decided that...

The difference is that Rust surfaces this reality to us, while other languages
try to hide it behind special mechanisms.

---

## Spare a Thought for Your Error Data

Given that our errors are data, and Rust makes us handle that data explicitly,
perhaps we should care about it - really care about it - and put just as much
thought and attention into how error information flows through our system as
we do for the happy path data.

Look after your error data... it'll look after you.

---

<details>
<summary><i>A note on how this post was written</i></summary>

_Disclaimer: I hadn't posted in this blog for many years mostly because I didn't
get round to it.  I had ideas for blog posts, but didn't find the time to write
them out.  For a while I also was doing technical blogging on the website
for the company I worked for, so got my "technical writing fix" from that.  Despite
that, I've had years at a time where I didn't write any technical blog, which
I regret, as I think it is good for the mind and good for the soul.  To kickstart
me getting some of my thoughts down on paper again, I'm experimenting with using
Claude code to assist me.  I've tried developing a "skill" by getting it to process
all my previous blog and all my writings in Slack to try to capture my "voice".
I'm giving it a stream of conciousness notes and asking it to write that into prose
in my voice.  So these posts are "written by AI", but only with ideas I've told it
to write, and hopefully in my voice._

**Raw notes this post was derived from:**

```
# "There's No Such Things as an Error in Rust"

colleagues have heard me say this so I'll explain.

start with a brief explainer on how errors are handled in rust, summarising
that they are done so through an algebraic data type.

Go off on a tagent about how rust calls them "enums" when they are not
actually enums but instead "sum types".

point out that to rust the "error" is "just data".

contrast to languages like java and python that have "execptions" that need
to be caught.  Describe this as a hidden subsystem / subway of information
flow that you can't see on the page in front of you when you read the code.

whereas in rust everything is explicit, things aren't hidden from you.  you
are exposed to the full complexity of what you're doing, always... even when
that really hurts...

revisit the point that in rust your error is "just data", and expand to point
out how the `Result` enum is arbitrary and "just a convention".  Other than
the `?` operator there's nothing special about it.  you could define your own

enum Either<L, R> {
    Left(L),
    Right(R),
}

and choose a convention that "left" is where you put your error information.

ultimately error data is almost a first class citizen.

the point I'm trying to make with "in rust errors are just data" is that it's
not just in rust... to computers "errors are just data"... "errors are just data".
The whole concept of an error is a human one that we've imposed over data, the
computer just sees 1s and 0s.

(as a final remark) given that our errors are data, and rust surfaces that to
us and makes us handle it and its flow through our application, perhaps we should
care about that data, and put just as much care an attention into how it flows
through our system as the "Ok" variant data.  Look after your error data.
```

</details>
