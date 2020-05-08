+++
title = "Re-Learning Haskell with Advent of Code - Part 3"
date = 2020-04-01T16:35:26+01:00
images = []
tags = []
categories = []
draft = true
+++

# finishing AoC, or Revising old code?

# general observations

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


