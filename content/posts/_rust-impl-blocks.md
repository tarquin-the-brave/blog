+++
title = "Rust impl Blocks - Do We Need 'em?"
date = 2020-12-04T15:43:16Z
images = []
tags = []
categories = []
draft = true
+++

- first thread
  + methods in impl re-implmenting traits
  + joys of polymorphism
- 2nd thread
  + trait design
  + keeping traits small
  + single purposed - better "representing a property/interface that a type can
    posses"
  + Squint a bit - "It's the I in SOLID"

- So what are impl blocks
  + a degenerate trait
  + a statement of "this is an interface unique to my type"
    * "therefore no other type can exhibit the same properties" - as far as the
      type system knows anyway.
  + If we're trying to encode as much information about our program into our
    types as possible, is this not just a lie?

- so do we need 'em?
  + seems obvious to say:
    - "look for traits that describe the interface you want"
    - "Thing about your types in terms of what properties/interfaces you
      want/need them to have"


