+++
title = "Re-Learning Haskell with Advent of Code - Part 2"
date = 2020-04-01T16:36:28+01:00
images = []
tags = []
categories = []
draft = true
+++

_In [Part 1][part1], I skipped the day 2, 5, & 7 problems that get you
to build and use a Intcode computer.  Each problem provides an Intcode
program to run: A series of integers that can each either represent an
instruction or data, and can mutate itself.  Looking forward, every
odd number day from day 5 onwards uses the Intcode program.  So I
decided to come back to them, and make a concerted effort at a few of
them in a row._

# Intcode computer - Stateful Computation

## An Intcode Program

[Day 2][day2] introduces an intcode program that's essentially a
series of integers where you start at the beginning and perform some
operation depending on the integer (instruction) you find which may read or write
another element in the list, take an input, or produce an output, and
you then move along the sequence to the next instruction, which is
a number of elements along depending on the instruction you just ran.
Eventually you reach an instruction to terminate the intcode program,
or program crashes from some operation failing, finding an unrecognised
instruction, or reaching the end without terminating.
The explanation in [the problem description][day2] goes into more
detail with examples.

It looks like the general problem here is we've got some state that mutates
during computation (a run of the program) and we may need to keep the current
state around while we do some other calculation.

With mutation not really being a feature in functional languages I looked around
for how this can be modelled in Haskell.  From the looks of it, it sounded like
the [State Monad][wikistate] would be needed at some point.

### Initial Implementation

The [day 2 problem][day2] asks you to build something that will run a simple
intcode program with instructions `1` & `2` that each take the next three
integers as parameters and mutate an element of the intcode, and `99` that
terminates the program.  The problem asks you to run the intcode it gives
'til it terminated and give the first value in the intcode.

For this simple case, we're not concerned about inputs or outputs of the
intcode program and we can run in once and throw it away.

Roughly following the approach I used in the problems tacked in [Part 1][part1]
I started by thinking about what data matters.

```haskell
data Program = Program {
  intCode::[Int],
  status::Status,
  -- Instruction Pointer: ip
  ip::Int
} deriving(Show)

data Status = Running | Terminated | Crashed deriving(Show)
```

So we've got: the index that the current instruction is at, `ip`; the
status of the program, whether it's `Running`, `Terminated` (by a `99`),
or `Crashed` for some reason; and the `intCode` itself.

I could have done less than this on a first pass, only modelling
the `intCode` and `ip`, and letting the code crash if an operation
failed, but I was keen write safe code by using safe operations,
such as those from [`Data.List.Safe`][safelist] handling errors, hence the
`Crashed` variant of `Status`.

I've modelled the intcode as a list, `[Int]`. lists aren't efficient
for looking up values by index, which is done a lot in the solution,
but for now, I'm not hugely worried about performance at this stage.
If it becomes an issue in later problems, I can look into alternative
representations.  If not I'll cycle back through at some point and
try to optimise performance as an exercise.

From here I was able to write a function that takes the above state
and evolves it by one instruction, and running an entire intcode
program by recursing until the status is no longer `Running`.

```haskell
runProgram :: Program -> Program
runProgram prog = case status prog of
   Running -> runProgram . runStep $ prog
   _ -> prog

runStep :: Program -> Program
runStep prog = ...
```

`runStep` matches the instruction found at the instruction pointer
index in the intcode and runs the appropriate operation to produce
a new `Program` with a mutated intcode and the instruction pointer
moved along.

### Introducing the State Monad

The above recursive function proved to be enough to get the answers
to both parts of [day 2][day2].  As I didn't need to keep track of
the program state, the State Monad wasn't needed.

Anticipating I'd need it later, I tried wrapping the
computation in the State Monad to see how it works.

From reading the [State Monad docs][docsstate], and with some help
for the [chapter in Learn You a Haskell][lyahstate], I've understood
it as: you have your state, `s`; the output of a computation on that
state, `a`; and a function that takes the state and returns the output
and the state evolved, `s -> (a, s)`.  You then use `state` to put
the function into the State Monad, denoted `State s a`.  Then it can
be manipulated with functions from [the docs][docsstate] and inside
`do` notation.

In this case the state type, `s`, is `Program`, and the output type,
`a`, is `()` as we're not concerned about the output.

```haskell
runProgram :: State Program ()
runProgram = do
  prog <- get
  case programState prog of
    Running -> do
      runStep'
      runProgram
    _ -> return ()

runStep' :: State Program ()
runStep' = state runStep

runStep :: Program -> ((), Program)
runStep prog
```

I was then able to create a `Program` with the intcode provided, pass
it to `runProgram` and use [`execState`][docsstate] to get the
final `Program` state.

The fact that I've used `()` for the output type is a sign that
the State Monad was not required here,  but it was good to do as an
exercise.

### Expanding the Intcode Computer

Over days [5][day5], [7][day7], & [9][day9] you're asked to build up the
intcode computer with a host of new instructions and the ability to take inputs
and give outputs.

Now there was a potential opportunity to use the State Monad in the intcode
computer, changing the output type and having the functions that return the
State Monad take a parameter.  E.g. if our input and output were both `Int`
we might have:

```haskell
runProgram :: Int -> State Program Int
```

In the case of these problems we need:

- A list of inputs, `[Int]`,
- A list of outputs, `[Int]`,
- Inputs are called one by one, not necessarily by the first instruction in the program,
- Outputs build up over the course of the program running.

As the program runs each instruction, we want to keep track of the
remaining inputs and the outputs thus far.  So I decided to include
input and output in the `Program` type, and not use my State Monad
implementation.

Including these in the type that tracks the state, along with a "Relative
Base" which the Instruction Pointer is taken as being relative to, we get:

```haskell
data Program = Program {
  input::[Int],
  intCode::[Int],
  status::Status,
  -- ip: Instruction Pointer
  ip::Int,
  -- rb: Relative Base
  rb::Int,
  output::[Int]
} deriving(Show, Eq)

data Status = Running | AwaitInput | Terminated | Crashed deriving(Show, Eq)
```

`runProgram` mostly stayed the same, with the case statement adding a match
for the `AwaitInput` variant of `Status`.

```haskell
runProgram :: Program -> Program
runProgram prog = case status prog of
   Running -> runProg . runStep $ prog
   AwaitInput -> if (length . input $ prog) > 0
     then runProg . runStep $ prog
     else prog
   _ -> prog

runStep :: Program -> Program
runStep = ...
```

### Adding Tests

As every odd day from 5 onwards uses this intcode computer, I defined
it in it's own module in a different directory and write some tests for
it.

TODO

### Using the Intcode Computer

I used this intcode computer implementation for the problems up to [day 15][day15].

TODO

## Making a Monad

In my intcode computer, there was lots of opportunity for various
operations to fail if the intcode was bugged or an input was bad.
I was modelling these with [`Maybe`][maybe], matching on `Nothing`, and
setting the program status to the `Crashed` variant of `Status`.

I was doing this _a lot_, essentially writing the same 3 lines of code
over and over.  This indicated to me that there was an abstraction
to be made[^dry].

### Failing Functions being Generic over MonadFail

The general case of this is where we have a type
that can represent failure, and if an operation fails we want
to return that type representing failure.

As an example, say we want to return a list where an empty list
represents failure.  In this simple example a negative input
results in failure.

```haskell
maybePos :: Int -> Maybe Int
maybePos x
  | x < 0 = Nothing
  | otherwise = Just x

thing :: Int -> [Int]
thing x = case maybePos x of
  Nothing -> []
  Just posX -> [posX]
```

`thing` has to match on the `Maybe Int` given by `maybePos` and
return either the `Int` in a list or an empty list.

I found myself repeating this patten:
* Model the operation that can fail with a function that returns `Maybe`,
* Match on the `Nothing` and return my failure type.

I was doing a lot of manually converting one type's failure representation
to another type's failure representation.  This seemed to me like there was
something I could use to do this automatically, as I was expressing the same
thing over and over again.

In the example above, both `Maybe` and `List` are monads.  I'm converting
from one monad's failure representation to another.  After a bit of looking
around I found the [`MonadFail` typeclass][monadfail].  As `Maybe` and `List`
both are instances of `MonadFail` we can redefine `maybePos` to be generic
over `MonadFail` and cut out the conversion between failure representations.

```
maybePosM ::  MonadFail m => Int -> m Int
maybePosM x
  | x < 0 = fail "woops"
  | otherwise = return x

thing' :: Int -> [Int]
thing' = maybePosM

posIntoMaybe :: Int -> Maybe Int
posIntoMaybe = maybePosM

posIntoIO :: Int -> IO Int
posIntoIO = maybePosM
```

### Refactoring the Intcode Computer

TODO

### Testing Monad Laws

TODO

# Other Things That Came Up

## Recursion

In [my last post][part1] I said:

> I’m not sure why, but Haskell make recursion feel like a natural way to
  solve problems. I use recursion in other languages, but it always feels
  like I’ve done something a bit clever.

I think I know why now:

> Functional purity allows you to recurse with confidence.

It comes down to: how much information you have to hold in your head while
following the code.

In procedural languages, where methods have side effects, or at least are
not guaranteed not to, you need to keep in mind all the state that
can be affect when following a method.  When a method recurses,
you then have to consider all the state that the method can affect on
that recursion. You can't treat each recursion symmetrically, because each
recursion adds to the number of things you need to consider.

With functions[^functions], you only need to consider their inputs.
And importantly you only need to consider their inputs as data, not as
"either representing data or a place to put data"[^plop].  When a function
recurses, you can leave all context that wasn't passed directly to the
function behind when reasoning through the recursion. You can
descend into the recursion carrying with you only what you
carried into the last one.

Also as functions return the same output given the same input, you
generally only need to reason through the recursion once.

By this same reasoning, I'd argue that a fold is simpler[^simple] than
a for loop that iterates over a collection (for languages that support
both and you have the choice).

When reasoning through a fold you only need to consider two values:
the accumulator; and the item taken from the collection.  It's almost
like being in a bubble, where only these two values matter.

In a for loop however, you're exposed to the context outside the loop.
You can't forget about any variables in scope, as you might change them
while looping.  You also have control flow to consider.  You might hit
a `break` or `continue` on a particular loop which means you then have
to skip over the code blow it that you would have otherwise reasoned
through (a `goto` in sheep's clothing).  Like with recursion in procedural
languages the number of things you need to consider grows as you iterate.
With functional purity that number of things to consider stays steady.

## Compiler Warnings

I turned on a bunch of compiler warnings, as per the advise of [this
article][opinionatedguide], by adding this to my `~/.stack/config.yaml':

```yaml
ghc-options:
  "$locals": -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints
```

I had initially thought that I wanted to use Haskell as "out of the box"
as possible, and not "depend on" extra lints to tell me what to do, but
then remembered that's a terrible argument.

In reality, when people employ extra linting and static analysis they
don't become dependent on it, they learn from it and get better at
writing better code first time, and continue to write better code when
the linting is turned off.

When you work in a language with a strong type system and have a compiler
that kicks your ass when you get it wrong, you become better at writing
correct code first time in all languages.

So I turned the compiler warnings on.  It did make ghci a bit spammy.
I'll see if there's a nice way to turn the warnings off for ghci.

## Breaking Functions Into Lots of Pieces

In a few of the Advent of Code problems, especially when I was eager to
get the problem done and move on, I found myself falling into a pattern
of:

* Knowing the data I want, and thinking about all the steps between that
  and the data I have.

I sometimes fall into the trap of:
* thinking procedurally
* having a complex thing to "do"
* splitting that up into lots of "functions"
* non of which make any real sense on their own.
* code is an unreadable mess.
`where` keyword is actually fixing this as it breaks the calculation down
without exposing all the sub functions to the rest of the program.

## Modules & Namespacing

In [my last post][part1] I talked about: not knowing how to find out
what package to install to get a certain library; and not being totally
comfortable with the namespacing when modules are imported and sufferring
from "where did that function come from?" syndrome.

The first of these was me just being dumb as it turns out the
package containing a module is written in the top left hand corner
of the module documentation's web page.  Spot `mtl-` in [the
docs][statemonaddocs] for `Control.Monad.State.Lazy`.

On the second of these: I've got reasonably comfortable with either
doing a qualified import to preserve namespacing, or being explicit
about which functions I'm importing if I'm importing them into the
module's namespace.

```haskell
import qualified Foo.Bar as FB
import Baz
  ( foo
  , fooBar
  , fooBarBaz
  )
```

I've found using a combination of these: qualified import and
explicitly listing the functions is a bit over the top.

I also practiced a pattern of having a module which renames
functions from another module when the names of functions and
types didn't make as much sense in the context I was using them,
or were just down right silly names in the first place.

```haskell
module Sensible
  ( foo
  , bar
  , Baz (..)
  , Silly.okName
  )

import Very.Silly.Named.Module as Silly

foo = Silly.longFooName
bar = Silly.otherBarName

type Baz = Silly.BazBaz
```

Using a combination of qualified imports and explicitly naming
functions I'm importing into the module's namespace, I've found
I can completely prevent the "where did that function come from"
syndrome.

In [my last post][part1] I made a lot of comparisons between how Rust
does things and how I was finding things are done in Haskell.
I made a general point that Rust is more explicit.  This is an example
of where Haskell can be more explicit.  In Rust, a lot of functionality
is provided via traits.  If a type implements a trait, you can call
the methods from that trait on that type by importing the trait.  The
methods themselves don't need to be explicitly imported.  The result is
that you can read some code that deals with a type you know (or at least
know where to find the docs for), that then calls a method on that type
that doesn't appear in the type's documentation.  You're left wondering
"where did that method come from?".  This is the part where I'd use
a "go to definition".  Without that you'd need to search through the
traits that are imported in scope as any one of them might have been
implemented for the type and try to find the method.  I try to avoid
this problem in Rust by keeping imports of traits as tightly scoped
with the code that uses them as I can so it's as obvious as possible
where the method came from.  This is probably one of the rare examples
where Rust is not [readable as text][idepost], where for the most part
it is pretty good at being.

# What Next?

TODO

- read more about Monad Transformers
- As I said last time - "I could probably use the tools I've got and grind away to solve all
  the problems" - Learning more Haskell, along with data structures, mathematics, and patterns
  that can help in all languages is the goal.
- Loop back through
  - perf
  - more generic

[valueofvalues]: https://www.youtube.com/watch?v=-6BsiVyC1kM
[simplemadeeasy]: https://www.youtube.com/watch?v=oytL881p-nQ
[opinionatedguide]: https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/
[part1]: https://tarquin-the-brave.github.io/blog/posts/re-learning-haskell/
[idepost]: https://tarquin-the-brave.github.io/blog/posts/ide-read-code/
[statemonaddocs]: https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Lazy.html
[day2]: https://adventofcode.com/2019/day/2
[day5]: https://adventofcode.com/2019/day/5
[day7]: https://adventofcode.com/2019/day/7
[day9]: https://adventofcode.com/2019/day/9
[docsstate]: https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Lazy.html
[wikistate]: https://wiki.haskell.org/State_Monad
[lyahstate]: http://learnyouahaskell.com/for-a-few-monads-more#state
[safelist]: http://hackage.haskell.org/package/listsafe-0.1.0.1/docs/Data-List-Safe.html
[maybe]: http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Maybe.html
[dryblog]: https://tarquin-the-brave.github.io/blog/posts/dry-not-a-goal/


[^functions]: I'm careful to not call something a function if it isn't a pure function.
              The ship sailed a bit on that one as many languages call their inpure methods
              "functions", but I find the distinction useful, not least to help context
              switch when moving between procedural and functional frames of mind.

[^plop]: [A great talk by Rich Hickey][valueofvalues] "The Value of Values" talks
         about PLOP "Place Oriented Programming".

[^simple]: [Another great talk by Rich Hickey][simplemadeeasy] "Simple Made Easy"
           talks about what simplicity is and how we can think objectively about it
           rather than it being a stand in for what people are most familiar with.

[^dry]: I wrote [a blog post recently][dryblog] about how removing repetition in code
        isn't the be all and end all, but is a good indicator that some abstractions
        are needed.
