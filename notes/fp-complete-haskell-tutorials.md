
## Notes

**bottom value**: when evaluated: runtime exception or infinite loop

**Total function** produces a non-bottom output for all non-bottom input.

**Partial function** may produce a bottom output for some non-bottom input.

### Applicatives

`liftA2`, `liftA*`... apply a function to `2` (or `n`) values wrapped in
applicatives.  Can use `<$>` & `<*>` instead.

`*>` throw away result on the left.  `<*` throw away result on the right.
(arrow points to the one you keep).

## RIO

"a haskell standard library"

### Record syntax setting

```haskell
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
  } deriving Generic

instance NFData RunningTotal
```

### Strict data

can put `!` in type def of struct fields

### Newtypes

`newtype`s have no runtime representation - like rust - they're there for the
type checker.  Therefore

```haskell
newtype Baz = Baz Int

main = Baz undefined `seq` putStrLn "Still alive!"
```

will crash.


### Operators `$!` is our friend - evaluate right strictly `$!!` do that deeply

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
- as keys are all they have they are Value Strict and do not have lazy
  varieties.

`Data.Sequence.Seq` - spine?

`fold` is not strict - use `fold'`

Functions
- Functions can be "strict in certain arguments"
- any "strict function" is only strict to WHNF
- use `force` to force into NF:

```haskell
import Data.List (foldl') import Control.DeepSeq (force)

average :: [Int] -> Double
average = divide . foldl' add (0, 0)
  where divide (total, count) = fromIntegral total / count
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

when mapping over the keys in a map - data can be lost if the operation is not
monotonic: one-to-one.  If an operation turns two keys into the same key, one
of them will be dropped.

## Vectors

- Storable - data is instance of `Storable` typeclass and is pinned (w.r.t GC),
  can be used for FFI
- unboxed - `Prim` typeclass, data is unpinned GC-managed

Pick vector type based on the typeclass the values are in. if neither `Prim` or
`Storable` use Boxed Vector.

vector package has "stream fusion" - avoid unnecessary array creation and uses
a tight loop where possible.

No instances of Functor, Foldable, Traversable for Unboxed & Storable Vectors

### Mutable Vectors

`freeze`, `thaw`, go to/from immutable vector via Copy.  `unsafeFreeze`,
`unsafeThaw` does this in situ.

ASIDE: lots of "unsafe is a bit faster but beware" - not "zero cost
abstraction" like in Rust.


# Monad Transformers

## `transformers` package

`ExceptT == EitherT`

- always make the base monad `m` the 2nd to last type parameter in monad trnasformer definition.  Allows `MonadTrans` to be defined.

stack of:
base monad -> monad transformer -> monad transformer -> etc...

`lift`

- lift an action in the base monad up to the monad the transformer is for.
- `MonadTrans` provides `lift`
- "lift" will wrap the inner monad in the outer.

## other stuff

`mlt` provides `MonadState` typeclass which allows us to call `get` & `put` on _any_
monad stack over `State`.

`State` is actually also a transformer, over the identity monad:

```haskell
type State s = StateT s Identity
```

No IO transformer

- IO has to be the base
- `mlt` gives us `MonadIO` with `liftIO` to lift IO actions into other monads.
  + I.E: functions that return `IO` can be lifted.

## Unlifting

What about when a function take IO as _input_?

- "function is _contravarient_ on IO"
- "funtion has IO in negative position"
- function cannot be generalized for use in another monad with `liftIO`.

Enter `MonadUnliftIO`

Something to come back to - plenty of links here.

# TODO - look into:

- functional dependencies
- `forall s.`


