+++
title = "We All Write Monads, Whether We Know It or Not"
date = 2020-05-04T04:46:31+01:00
images = []
tags = []
categories = []
draft = false
+++

Recently I've been rebooting my Haskell by working through [Advent of
Code][aoc], and bogging about it:

- [Re-learning Haskell with Advent of Code - Part 1][part1],
- [Re-learning Haskell with Advent of Code - Part 2][part2],

and it's got me thinking.  I was quickly re-introduced to the concepts of:
Functors, Applicative Functors, Monoids, Monads, a strong type system, higher
kinded types, and functional purity, and straight away, started spotting these
patterns in the code I write every day at work (predominantly Rust with some
Bash, Python, and C++ thrown in for good measure).

I find myself thinking about code I'm working with, and saying to myself, or
anyone who will listen, things like:

> "Ah well, this works because string concatenation behaves monoidally..."

or:

> "What we've got here is a computational context for our data that we want to
> map over..."

What this has made me realise is there's nothing special about Haskell that
means these concepts exist where they don't exist in other languages.

> Haskell formalises data manipulation patterns that exist wherever there is
> data to manipulate.

Have you ever had a "container type" for your data that you've iterated over?
You've used a functor.

Have you ever encoded error handling in the data you're using?  You've used
something resembling a monad.

Rust's ownership model is a formalisation of a set of memory management
patterns that C developer might employ to attempt to write memory safe and
thread safe code.

Regardless of the language we're writing in, these data manipulation patterns
are there. The language may formalise them, it may not. By formalising data
manipulation patterns in a language, there's the opportunity for the compiler
to tell you if you're using them right, otherwise you're performing these
patterns by hand.

What it comes down to is:

[![](https://mermaid.ink/img/eyJjb2RlIjoiZ3JhcGggVERcbiAgQVtEb2VzIHlvdXIgbGFuZ3VhZ2UgZm9ybWFsaXNlIHRoZXNlIHBhdHRlcm5zP11cbiAgQyhbVGhlIGNvbXBpbGVyIGtlZXBzIHlvdWggcmlnaHQgZmE6ZmEtdGh1bWJzLXVwXSlcbiAgQltBcmUgeW91IGF3YXJlIHlvdSdyZSB3cml0aW5nIHRoZW0gYnkgaGFuZD9dXG4gIERbWW91J2xsIHdyaXRlIG1vcmUgY29uc2lzdGVudCwgY29ycmVjdCwgYW5kIG1haW50aW5hYmxlIGNvZGUgZmE6ZmEtdGh1bWJzLXVwXVxuICBFW0J1Z3Mgd2lsbCBoYXBwZW4gZmE6ZmEtYnVnXVxuICBBIC0tPnxZZXN8IENcbiAgQSAtLT58Tm98IEJcbiAgQiAtLT58WWVzfCBEXG4gIEIgLS0-IHxOb3wgRVxuXHRcdCIsIm1lcm1haWQiOnsidGhlbWUiOiJkZWZhdWx0In0sInVwZGF0ZUVkaXRvciI6ZmFsc2V9)](https://mermaid-js.github.io/mermaid-live-editor/#/edit/eyJjb2RlIjoiZ3JhcGggVERcbiAgQVtEb2VzIHlvdXIgbGFuZ3VhZ2UgZm9ybWFsaXNlIHRoZXNlIHBhdHRlcm5zP11cbiAgQyhbVGhlIGNvbXBpbGVyIGtlZXBzIHlvdWggcmlnaHQgZmE6ZmEtdGh1bWJzLXVwXSlcbiAgQltBcmUgeW91IGF3YXJlIHlvdSdyZSB3cml0aW5nIHRoZW0gYnkgaGFuZD9dXG4gIERbWW91J2xsIHdyaXRlIG1vcmUgY29uc2lzdGVudCwgY29ycmVjdCwgYW5kIG1haW50aW5hYmxlIGNvZGUgZmE6ZmEtdGh1bWJzLXVwXVxuICBFW0J1Z3Mgd2lsbCBoYXBwZW4gZmE6ZmEtYnVnXVxuICBBIC0tPnxZZXN8IENcbiAgQSAtLT58Tm98IEJcbiAgQiAtLT58WWVzfCBEXG4gIEIgLS0-IHxOb3wgRVxuXHRcdCIsIm1lcm1haWQiOnsidGhlbWUiOiJkZWZhdWx0In0sInVwZGF0ZUVkaXRvciI6ZmFsc2V9)

When I first learned some Haskell a few years ago, the idea was sold to me as
"it'll make you better at every other language".  That was enough to sell it to
me at the time, and I believe it had that effect.  It's true of every language
to some extent.  Every language formalises some number of data manipulation
patterns, the question is to what extent, and are they good patterns to be
using to get correct and maintainable code.

I hear so much from developers who would answer "No" to both of the above, that
learning the concepts that formalise patterns of data manipulation is overly
esoteric and not pragmatic.  To try to understand these things makes you a
"purist", and gets in the way of "getting things done".  I think the reality is
that the software industry, as a whole, is not under enough commercial pressure
to write software that works with enough efficiency.  Developers can get away
with not needing to really understand what they're doing, unknowingly
handwriting attempts at the data manipulation patterns, and producing code
that's inefficient to write, expensive to maintain, and inevitably bugged.

As software becomes more prevalent in all industries, every professional is
writing code, and every child learns coding in school (which they already are):
this situation is going to change.  Software developers will no longer be able
to deliver broken things and convince everyone else that that's OK. In the
meantime let us hope that the bugs that we inevitably write due to this willful
negligence won't cost someone their life.

This isn't to say that there won't always be bugs in software.  No matter how
solid the tools you use are, you could have just missed the mark on
requirements and built the wrong thing.  But there are whole classes of bugs
and errors that can be eliminated by formalising these patterns and encoding
them into the fundamental tools with which we build things.

I work with Rust most working days.  While it's missing higher kinded types,
it's got a lot going for it with its strong type system, its state ownership
model, and generally how it makes you be explicit about what you're doing.
People say with Rust that "if it compiles it works", and provided you've
written code that leverages the type system, there's some truth to that. It
makes me hopeful for the future, and I believe that Rust will stand as one of
the cornerstones of a new generation of software that works.

[aoc]: https://adventofcode.com/2019/
[part1]: https://tarquin-the-brave.github.io/blog/posts/re-learning-haskell/
[part2]: https://tarquin-the-brave.github.io/blog/posts/re-learning-haskell-2/
