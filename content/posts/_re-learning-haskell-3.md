+++
title = "Re-Learning Haskell with Advent of Code - Part 3"
date = 2020-04-01T16:35:26+01:00
images = []
tags = []
categories = []
draft = true
+++

At the end of [Part 2][part2] I set out my next goals as:
- gain a more complete understanding of monad transformers and property based testing,
- cycle back around and improve the performance of my solutions, then
- carry on with a more complete toolkit at my disposal.

So I started search around to see what resources I could find.

[part2]: https://tarquin-the-brave.github.io/blog/posts/re-learning-haskell-2/

# (Re-)[^re]Learning Haskell with(out) Advent of Code

After searching around a little, I stumbled upon [the Haskell learning resources
shared by FP Complete][fpch]. There's so many goodies here!

[fpch]: https://tech.fpcomplete.com/haskell/learn

Ran through tutorials on:
- Applicative Syntax
- String Types
- Strictness
- Monad Transformers - todo exercises
- Safe Exception handling - todo
- Vectors - to finish

loads more there.

Let's see how I can improve the AoC solutions so far.

# Refactoring AoC Solutions

## Small Improvements

Some small improvements I picked up as handy tips or
on a 2nd look saw as obvious and trivial.

### Writing Stack Scripts

For the simpler problems, I didn't need the overhead of generating a full project
with `stack new` and opted to replace that with a single `dayX.hs` script which starts like:

```haskell
#!/usr/bin/env stack
-- stack --resovler lts-15.4 script

```

I'd gone with the full project structure given by the default template used by
`stack new` to get used to importing and exporting and to find the namespacing
control I was happy with.  I've done that now, so I can reduce some of the problems
to simple scripts and "get things done!".

### Using `where` More

As I elucidated on in [Part 2][part2], my earlier solutions were plagued with the proliferation
of small functions that don't really mean a huge amount on their own and are only called as part of
another function.  `where` helps to clean this up by putting them inside the namespace of the function
they're really part of and de-cluttering the namespace of the module.

### Use `foldl'` Over `foldl`

"For strict application of the operator" - [the docs say][folddoc].

[folddoc]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Foldable.html#v:foldl-39-

* use map rather than list for day3
* deriving instances

## Big Changes

### Stop Matching Maybes - More Monad Transformers

Use monad transformers instead.

### No More Strings

### Strictness

Day 1 solution before adding bang patterns:


### Vectors over lists

### Mutable Vectors to Model Intcode

### Making Types Generic

### Making Types Specific

### No Prelude

### RIO - maybe going forward...

all these improvements can be seen in this Monster PR...

# general observations

## Safety

## Not 1 way to do things

This is good.  I wonder how far Rust will get... TBF this is
reflected in Rust too. Just different things.


[^re]: thing
