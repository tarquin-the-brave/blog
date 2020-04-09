+++
title = "Re-Learning Haskell with Advent of Code - Part 2"
date = 2020-04-01T16:36:28+01:00
images = []
tags = []
categories = []
draft = true
+++


# Intcode computer - stateful computation

## An Aside on recursion

In my last post I said:

> I’m not sure why, but Haskell make recursion feel like a natural way to
  solve problems. I use recursion in other languages, but it always feels
  like I’ve done something a bit clever.

I think I know why now:
* Purity allows you to recurse with confidence.
* Cognitive complexity (clippy told me about this).

By the same reasoning a fold is less complex than a for loop.

link "simple made easy" by Hickley

## Aside on compiler warnings

turned on extra compiler warnings after day7
* didn't want to be depdendent on them
* but that's actually a shit argument
  + after a short while you start writing things well be default.
  + or, in rust do I just write any old balls and let the compiler work it out for me?

# the problems

state monad for when we care about the evolution of state.

intcode program didn't end up wanting a state monad because it didn't care
about it's evolution really.  It churn some numbers then stops when it fails
or exits of needs more input.

intcode module describes states and transformation between them, but doesn't
need to "track state".

# general observations

I sometimes fall into the trap of:
* thinking procedurally
* having a complex thing to "do"
* splitting that up into lots of "functions"
* non of which make any real sense on their own.
* code is an unreadable mess.
* happens when I'm rushing/world outside my head put demands on me (girlfriend)

because there's no objects, namespacing is a bit funky.
e.g. non-empty list has most of the list methods but needs
a `qualified` import.
* I suppose without objects you need some way of namespacing.
  That's all "objects" are really.
  + Procedures in the same namespace as data.
  + I'm always disappointed to find out how "not a thing" a thing is.

Safety:
* I want to write "safe" code
* in Rust you have to be explicit both:
  + when you want to do something not "memory safe" - haskell is GC, not a problem
  + when you want to do something that "may crash/panic" - akin to haskell notion of safety"
    - e.g. you got to stick `unwrap()` or `expect()` around the place.
* I'm finding in Haskell
  + safe code comes with faff
  + so if you know your usage won't touch an unsafe case, should you still make the effort?
    - for the code itself, it's probably pointless,
    - but what about extensibility/maintainability?
    - should we just write safe code all the time and accept the cost of doing so as:
      * a) part and parcel of writing decent code,
      * b) worth it in the medium/short term?
    - a down side of learning via these problems is there's not the drive to make code maintainable,
      you don't necessarily get the lessons about maintenance.
    - you do have to extend code though, so I suppose extensibility comes into it.

generality
* generality is good right?
* but means breaking changes to library - e.g. `replaceNth` went from:
  + `Int -> a -> [a] -> [a]`, to:
  + `Num a => Int -> a -> [a] -> [a]`

