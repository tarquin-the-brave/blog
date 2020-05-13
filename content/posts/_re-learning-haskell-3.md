+++
title = "Re-Learning Haskell with Advent of Code - Part 3"
date = 2020-04-01T16:35:26+01:00
images = []
tags = []
categories = []
draft = true
+++

# Re-learning Haskell with(out) Advent of Code

found FP Complete's tutorials - amazing!

## Notes

**bottom value**: when evaluated: runtime exception or infinite loop

**Total function** produces a non-bottom output for all non-bottom input.

**Partial function** may produce a bottom output for some non-bottom input.

### Applicatives

`liftA2`, `liftA*`... apply a function to `2` (or `n`) values wrapped in applicatives.
Can use `<$>` & `<*>` instead.

`*>` throw away result on the left.
`<*` throw away result on the right. (arrow points to the one you keep).

## RIO

"a haskell standard library"

### Record syntax setting

```hakell
data Thing = Thing { x::Int, y::Int }

let a = Thing 1 2
-- you can do this
let a' = a { y = 3 }
```

### Lenses

Package a getter and a setter for a data structure field.

`view`, `set`, `over` (equivalent of modify).

### Lifting and unlifting

`liftRIO` can convert between RIO function and more generic mtl style

### rio final exercise

experience of RIO stack template.

RIO is a way of doing things.  Gonna roll with RIO for now, but want to
experience the different ways of doing things.

### Strictness

Haskell is Lazy

thunk - promise of calculation - like a future?

when we need it we _force evaluation_

* avoids unnecessary work
* thunks needs space allocated, can be a lot more for chained computation
* Haskell does have _strictness analysis_ to find times when it is far cheaper
  to be strict.

#### Bang

put a `!` in front of arguments: `add !x !y = x + y`

syntactic sugar for `seq` operator:

```haskell
add x y = x `seq` y `seq` x + y
```
"evaluate x then y the x + y"

Bottom Values: `undefined`

Thunks are essentially closures.  Closures make things lazy.

`Debug.Trace` is useful

#### Weak Head Normal Form

`seq` evaluates to WHNF, i.e. unwrap on layer of constructor

`deepseq` evaluates all the way down

`NFData` typeclass "normal form data" - can derive with `Generic` deriving

```haskell
{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics (Generic)
import Control.DeepSeq

data RunningTotal = RunningTotal
  { sum :: Int
  , count :: Int
  }
  deriving Generic
instance NFData RunningTotal
```

### Strict data

can put `!` in type def of struct fields

### Newtypes

`newtype`s have no runtime representation - like rust - they're there for the type checker.
Therefore

```haskell
newtype Baz = Baz Int
main = Baz undefined `seq` putStrLn "Still alive!"
```
will crash.


### Operators
`$!` is our friend - evaluate right strictly
`$!!` do that deeply

### Data structures

Lists - Lazy

Spine strict - we know all the details about the shape of the data structure
```haskell
data List a = Cons a !(List a) | Nil
```

Value Strict - we also know all the values
```haskell
data List a = Cons !a !(List a) | Nil
```

Vectors
- `Data.Vector` - boxed vectors - Spine Strict
- `Data.Vector.Unboxed` - unboxed vectors - Value Strict

Maps
- Strict and Lazy varieties
- "lazy" variety are still Spine Strict - strict in keys

Sets
- as keys are all they have they are Value Strict and do not have lazy varieties.

`Data.Sequence.Seq` - spine?

`fold` is not strict - use `fold'`

Functions
- Functions can be "strict in certain arguments"
- any "strict function" is only strict to WHNF
- use `force` to force into NF:

```haskell
import Data.List (foldl')
import Control.DeepSeq (force)

average :: [Int] -> Double
average =
  divide . foldl' add (0, 0)
  where
    divide (total, count) = fromIntegral total / count
    add (total, count) x = force (total + x, count + 1)

main :: IO ()
main = print $ average [1..1000000]
```

`foldl'` is only strict to WHNF.

## Data structures

### String types

ByteString - binary data - "off the wire" Text - texual data

OverloadedStrings
- string literals to be treated like arbitrary string types
- `pack` does this explicitly

act like lists - except for:
- monomorphic
  - not HKT
- strict
- smaller memory overhead than lists - packed into a memory buffer
- finite data structures

Converting between `ByteString` & `Text`
- `Data.Text.Encoding (decodeUtf8, encodeUtf8)`
- `decodeUtf8` is partial! Instead use either:
  + lenient decoding `Data.Text.Encoding.decodeUtf8With
    Data.Text.Encoding.Error.lenientDecode bs` (`bs :: ByteString`)
  + `decodeUtf8'` returns an `Either`

Don't bother with lazy varieties.

IO
- stdin/stdout/stderr - use `Data.Text.IO`
- files/sockets/etc. - use `Bytestring` based functions - `Text` functions
  respect encoding of environment which might not exist in a docker container.


**FFI & representation**

Text:
- UTF-16
- Unpinned memory
- Can't do FFI

Bytestring:
- binary
- pinned
- convert to bytestring for FFI


**Builders**

rather than:
```haskell
"Hello " <> "there " <> "world" :: Text
```

do:
```haskell
toLazyText ("Hello " <> "there " <> "world" :: Builder)
```

Same for bytestring.

bytestring also has streaming functions.

prefer `Word8` over `Char8` unless you know the data is ascii - HTTP headers


## Containers



# Refactoring AoC Solutions

## List of Improvements

Of the back of doing these tutorials and what I can remember in my
solutions I know I'll make changes:
* use `foldl'` over `foldl`

## What have I learned that will help?

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


