+++
title = "Why I Scatter Use Statements Throughout My Rust"
date = 2020-07-15T15:35:41+01:00
images = []
tags = []
categories = []
draft = true
+++

A standard pattern across pretty much every language I've worked with at least,
is to stick statements that import modules and libraries, at the top of the
page.  Some languages make you do this.  In Rust I don't do this for
everything, here's why.

I started to think more and more about what code is like to read rather than
write. I for one was reading code far more often than I was writing, and the
code I was writing would, over its lifetime, be read far more often than
written or edited.

To set the scene, take the following code:

TODO: Example

While this example is quite simple, you can imagine this file having more and
longer functions and not fitting onto one page.  Suddenly you're eyes are
jumping up and down the page trying to cross reference types you're not
familiar with.  Next you see a method call you're not familiar with on a type
that you are.  You go to the type's documentation and the method is nowhere to
be seen.

Suppose I re-wrote this code to:

TODO: Example

[ide-rant]: ./ide-read-code.md

# Namespace Hygiene

This started for me with `main.rs`.  It's a first port of call for someone
reading application code for the first time and I decided I wanted readers
to find `fn main()` and have what they need in front of them.  As I was pulling
the `use` statements for the things used inside `main` into `main` it would
be silly and confusing to have any other functions not do the same and take from
the top level namespace. The result of this exercise is that each function in
`main.rs` serves as its own little island that can be visited without hopping
around the page to cross reference imported things, and the top level namespace
is clean.

While for code in `main.rs` I was playing with creating little islands to try
out the idea, without worrying too much about having a sound justification to
do so, when this idea of "namespace hygiene" is extended to the rest of your
code: it can have some quite profound implications on the simplicity and
robustness of the code.

TODO: further on simplicity, what's in scope.
When I think about complexity of code I try to look at things like: how many
moving parts are at play, how many different logical outcomes there are, how
many different paths of code flow go through the code (I believe that last one
is referred to as "cyclomatic complexity").  These are all things that we can
quantify and through which can have a meaningful discussion about complexity,
without conflating complexity with subjective judgements of how easy or hard
it is for any one individual to read or follow, given their unique life up to
that point. That's not to say that metrics of complexity should be the only
guiding principle to how we write code.  There's are elements of style, personal
expression, and story telling that go into code to communicate ideas.  But
when complexity is what's being discussed, it serves to have things that can
be measured.

Possibly the main huge benefit of namespace hygiene is refactoring. You want to
move some code around, how nice it is when your code blocks are **portable**.
Being minimally dependent on a higher namespace makes moving code so much
easier.

# Where Did That Function Come From?

For me, this is the one pet peeve I have with Rust's syntax/semantics.

> It's too easy to magically import methods via traits in a way that
  makes tracking down the documentation for them very hard.

In the above example I covered the case where you see a type you know and love,
but then see a method call on it that you're not so familiar with. You go the
that type's documentation, and this mysterious method is nowhere to be seen.
Where do you go from there? You search the `use` statements at the top of the
page for anything that isn't reference below, hoping that the writer of the
code has fixed build warnings about unused imports: you can take this as a sign
it's a trait.  You then exhaustively search the documentation of each trait
until you find a method of that name.  Unfortunately a method of that name
appears under two of the traits you've looked up.  Thankfully the orphan rule
can help you out here.  If the crate defining the type had implemented a trait,
the method would have appeared in the type's documentation.  If not, the crate
defining the trait must have implemented it for the type, and the fact it was
implemented for the type will appear in its documentation.  So you scroll down
to search of an implementation on your type in each trait's documentation and
finally you find the documentation you were looking for.  Whilst sitting back
in you chair to take a moment to admire your detective skills you realise it's
half an hour later, your tea's gone cold, and all you wanted to do was read a
line of code.

So this happened to me a few times.  While I wax and wane on how much I stick
to keeping imports of functions, macro, and types close to where they're called
vs.  just putting them at the top like everyone else, I do make sure to put
`use`s of traits as tightly scoped to their usage as possible.  While it
doesn't fully prevent the above scenarios it does really help.

It's also very helpful to mark an import as a trait by importing it to a hole.

```rust
use anyhow::Context as _;
```

Given that you're not going to do this for anything that isn't a trait, it
serves as a pretty strong indicator.

When you see a method you don't recognise and just above it an import of a
trait, it's a pretty good indication of where the method came from.

If playing around with clearing out the top level namespace in `main.rs` was
what got me started with moving `use` statements around, this issue is what
really convinced me it was a good idea.

# Code Is Text

> "Is the answer not to just bring up the code in you IDE and hover your mouse
  over it"?

I've already had a rant about this in [a previous post][ide-rant], so I'll not
launch into this one again. Essentially IDEs are there to make code easier to
write and can make code completely unreadable through a web GUI or a vanilla
text editor.  And code is mostly read through a web GUI whether that be in the
form of pull requests or visiting the source control of a library or
application.

# Taking This Further

Rust lets you define scopes within functions.  This gives us the opportunity to
make the things we import only be accessible in the tightest scope possible.

TODO: example

I tend not to do this in practice.  I might if it's an especially large
function where a bunch of imports are only relevant to a small section, but
generally Rust doesn't need yet another reason to have yet another level of
indentation.

# Not Importing At All

For one off uses of things, especially for less often used parts of the
standard library or crates that no-one's ever heard of, I tend not to use a
`use` at all.

If it's a one-off use, then why pollute your namespace with it? If reader
immediately break their flow of reading through your code to lookup where an
unfamiliar thing comes from, why not detail where that came from inline to not
break the reader's flow?

There are also lots of cases where the path to the thing you're importing
provide vital context for what that thing represents.  Take `semver::Version`
for example: the fact that it's "semver" is what's important.  Being semver is
what gives it all the properties of semver versioning.  Without semver, or an
alternative versioning scheme, a version is no more than an arbitrary text
field.  Basically mentioning that it's semver is important.  For things like
these I never use a `use` statement.

There [a clippy lint][mnr] in the pedantic group, for not repeating module
names in names of things. That idea doesn't appear to have caught on that well
in most crates as the path to things can usually be thrown away.  If more
crates did this we'd probably find cases like `semver::Version` more often.

[mnr]: https://rust-lang.github.io/rust-clippy/master/#module_name_repetitions

In fact, putting the full paths to things is where I tend to start, only later
employing `use` statements.

# When I Don't Do This

For small, truly single purpose modules, I pop all the `use` statements at the
top of the page.  If all the code in the file is performing roughly the same
role, and you can see it all on a page, the same concepts are going to be
familiar to all the code and there's not any overhead of context switching.

For very commonly used and widely recognised things, like
`std::collections::HashMap` I tend to just put a use for at the top of the
file.  No-one who's vaguely Rust literate is going to see `HashMap` and wonder
what it is.  `std::collections::VecDeque` however, I might not put a `use` for
at the top of file.

# Only Doing This In Some Places

It's all well and good when writing a new module for the first time to adopt
this style, or any other style you want. But when you come to working on
existing code, it often serves to stick with the style of the code already
there.

I've found this isn't so true with this issue of moving `use` statements away
from the top or not using them at all.  When you start doing this in code that
defines all its imports at the top, it doesn't wreck the consistency or create
confusion.  I've found it's just one less place where the reader has to jump
around and cross reference as they're reading.

# A Word On DRY

[I've blogged before about how the pursuit of DRY above all other things can
lead to bad software](./dry-not-a-goal.md).  I'd anticipate a counter argument
to moving imports closer their use or calling things by their full path is that
it's repetition.  Putting `use` statements at the top of the file means you
only write them once.  But you can take that argument to absurdity.  Why not
then have everything in the same namespace with all you code in one `main.rs`
file, then you only every have to import things once?  Even without that, I
think this serves as a good example of where DRY isn't the be all and end all.

---

I don't have the strongest of convictions about all of this, except [importing
traits](#where-did-that-function-come-from%3F), but overall it's a pattern I've
found useful.  And the portability really helps. It won't change the world, but
hopefully makes someone's life just a little bit better when they come across
some of my code years down the line.
