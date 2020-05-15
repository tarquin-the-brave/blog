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

# Re-learning Haskell with(out) Advent of Code

After searching around a little, I stumbled upon [the Haskell learning resources
shared by FP Complete][fpch]. There's so many goodies here!

[fpch]: https://tech.fpcomplete.com/haskell/learn

Ran through tutorials on:
- Applicative Syntax
- String Types
- Vectors
- Monad Transformers
- Strictness
- Safe Exception handling

loads more there.

Let's see how I can improve the AoC solutions so far.

# Refactoring AoC Solutions

## Small Improvements

Some small improvements I picked up as handy tips or
on a 2nd look saw as obvious and trivial.

* use `foldl'` over `foldl`
* use map rather than list for day3

## Big Changes

### Stop Matching Maybes - More Monad Transformers

Use monad transformers instead.

### No More Strings

### Strictness

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
