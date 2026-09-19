+++
title = "Rust Perspectives for Experienced Engineers"
date = 2026-01-09T11:11:59Z
images = []
tags = ["rust"]
categories = []
draft = false
aliases = ["/posts/rust_perspectives/"]
+++

About a year ago I joined a company.  They'd brought me in as "someone with
lots of Rust experience".  From the interviews I thought they'd have a few
projects here and there in Rust.  Turns out it was almost the entire stack for
their new product.  A lot of experienced folks from other languages were
learning Rust for the first time while pushing to ship.

Watching how people write Rust when coming from other languages, I thought
about what perspectives might be useful to share.  Ways of seeing things that
make Rust click a bit easier.

---

## It's Data Oriented, Not Object Oriented

You don't need "objects".  Rust has structs and enums and implementations, but
the mental model of objects with encapsulated state and methods that mutate
that state... you can mostly let go of that.

Errors are just data too.  They deserve the same thought and care as your
happy-path types.  I've written about this [elsewhere](/posts/no_errors_in_rust/).

## Everything Is a Value

It's all about data.  Values flow through your program.  This supports writing
very direct code - you can often just follow the data and understand what's
happening.

## Think About Stack and Heap

In many languages you don't really think about where your data lives.  In Rust
it matters.  Getting comfortable with what goes on the stack versus what gets
boxed onto the heap will help you reason about ownership and lifetimes.

## Everything Is Off by Default

Rust doesn't give you things you didn't ask for.  Traits aren't implemented
unless you derive or impl them.  Fields are private by default.

To be fair, I think private-by-default for struct fields was a mistake.  It
implies these are objects with invariants to protect, when often they're just
data.  But that's how it is.

The principle though: don't turn things on for no reason.  If you don't need
`Clone`, don't derive it.  If you don't need `pub`, don't add it.

## Code Is Text

Turn off your IDE for a bit.  Well, not completely - but try reading Rust
without leaning on go-to-definition and type hints everywhere.

Yes, it's more explicit.  Yes, it's more verbose.  But there's very little
hidden magic.  What you see is mostly what you get.  That explicitness is the
point.

## There Is No One "Rusty" Way

Don't let anyone tell you there's a single correct way to do something in Rust.
There are usually about ten ways to do anything, and they're all valid.
Different trade-offs, different contexts.

The community can be a bit prescriptive sometimes.  Take it with a pinch of
salt.

## Watch Out for Blocking I/O in Async

Rust doesn't protect you here.  If you call a blocking function inside an async
context, you'll block the executor and bad things happen.  The compiler won't
warn you.

## It Punishes Premature Abstraction

Rust's strong type system means if you make naive abstractions early on, the
language will hold you to them.  When those abstractions turn out to be
unsound, life gets hard.  You'll be fighting the compiler instead of working
with it.

Better to start concrete and extract abstractions when you actually need them.

## It's Going to Be Frustrating Sometimes

The ambitious goals of the language have come with compromises.  It's not
perfect.

Ease of use is probably 90% familiarity.  The other 10% is subjective and
depends on what time horizon you're thinking about - some things are easy at
first and hard later, others the reverse.

The compiler is your friend.  Take the time to read and understand the errors.
Don't just do what it suggests straight away - you could end up going round in
circles.  Understand *why* it's complaining first.

These aren't rules, just perspectives that helped me.  Rust rewards patience
and a willingness to see things a bit differently.

---
