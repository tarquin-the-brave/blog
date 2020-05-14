+++
title = "Re-Learning Haskell with Advent of Code - Part 2"
date = 2020-04-29T20:36:28+01:00
images = []
tags = []
categories = []
draft = false
+++

_In [Part 1][part1], I skipped the day 2, 5, & 7 problems that get you to build
and use a Intcode computer.  Each problem provides an Intcode program to run: A
series of integers that can each either represent an instruction or data, and
can mutate itself.  Looking forward, every odd number day from day 5 onwards
uses the Intcode program.  So I decided to come back to them, and make a
concerted effort at a few of them in a row._

# Intcode computer - Stateful Computation

## An Intcode Program

[Day 2][day2] introduces an intcode program that's essentially a series of
integers where you start at the beginning and perform some operation depending
on the integer (instruction) you find which may read or write another element
in the list, take an input, or produce an output, and you then move along the
sequence to the next instruction, which is a number of elements along depending
on the instruction you just ran.  Eventually you reach an instruction to
terminate the intcode program, or the program crashes from some operation failing,
finding an unrecognised instruction, or reaching the end without terminating.
The explanation in [the problem description][day2] goes into more detail with
examples.

It looks like the general problem here is we've got some state that mutates
during computation (a run of the program) and we may need to keep the current
state around while we do some other calculation (calculating the next input
of the program).

With mutation not really being a feature in functional languages I looked
around for how this can be modelled in Haskell.  From the looks of it, it
sounded like the [State Monad][wikistate] would be needed at some point.

### Initial Implementation

The [day 2 problem][day2] asks you to build something that will run a simple
intcode program with instructions `1` & `2` that each take the next three
integers as parameters and mutate an element of the intcode, and `99` that
terminates the program.  The problem asks you to run the intcode it gives 'til
it terminates and give the first value in the intcode.

For this simple case, we're not concerned about inputs or outputs of the
intcode program and we can run in once and throw it away.

Roughly following the approach I used in the problems tackled in [Part 1][part1],
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

So we've got: the index that the current instruction is at, `ip`; the status of
the program, whether it's `Running`, `Terminated` (by a `99`), or `Crashed` for
some reason; and the `intCode` itself.

I could have done less than this on a first pass, only modelling the `intCode`
and `ip`, and letting the code crash if an operation failed, but I was keen
write safe code by using safe operations, such as those from
[`Data.List.Safe`][safelist] handling errors, hence the `Crashed` variant of
`Status`.

I've modelled the intcode as a list, `[Int]`. lists aren't efficient for
looking up values by index, which is done a lot in the solution, but for now,
I'm not hugely worried about performance at this stage.  If it becomes an issue
in later problems, I can look into alternative representations.  If not, I'll
cycle back through at some point and try to optimise performance as an
exercise.

From here I was able to write a function that takes the above state and evolves
it by one instruction, and running an entire intcode program by recursing until
the status is no longer `Running`.

```haskell
runProgram :: Program -> Program
runProgram prog = case status prog of
   Running -> runProgram . runStep $ prog
   _ -> prog

runStep :: Program -> Program
runStep prog = ...
```

`runStep` matches the instruction found at the instruction pointer index in the
intcode and runs the appropriate operation to produce a new `Program`, with a
mutated intcode and the instruction pointer moved along.

### Introducing the State Monad

The above recursive function proved to be enough to get the answers to both
parts of [day 2][day2].  As I didn't need to keep track of the program state,
the State Monad wasn't needed.

Anticipating I'd need it later, I tried wrapping the computation in the State
Monad to see how it works.

From reading the [State Monad docs][docsstate], and with some help for the
[chapter in Learn You a Haskell][lyahstate], I've understood it as: you have
your state, `s`; the output of a computation on that state, `a`; and a function
that takes the state and returns the output and the state evolved, `s -> (a,
s)`.  You then use `state` to put the function into the State Monad, denoted
`State s a`.  Then it can be manipulated with functions [that act upon the State
Monad][docsstate], and inside `do` notation.

In this case the state type, `s`, is `Program`, and the output type, `a`, is
`()`, as we're not concerned about the output.

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

I was then able to create a `Program` with the intcode provided, pass it to
`runProgram` and use [`execState`][docsstate] to get the final `Program` state.

The fact that I've used `()` for the output type is a sign that the State Monad
was not required here, a function of `s -> s` would suffice,  but it was good
to do as an exercise.

### Expanding the Intcode Computer

Over days [5][day5], [7][day7], & [9][day9] you're asked to build up the
intcode computer with a host of new instructions and the ability to take inputs
and give outputs.

Now there was a potential opportunity to use the State Monad in the intcode
computer, changing the output type and having the functions that return the
State Monad take a parameter.  E.g. if our input and output were both `Int` we
might have:

```haskell
runProgram :: Int -> State Program Int
```

In the case of these problems we need:

- A list of inputs, `[Int]`,
- A list of outputs, `[Int]`,
- Inputs are called one by one, not necessarily by the first instruction in the
  program,
- Outputs build up over the course of the program running.

As the program runs each instruction, we want to keep track of the remaining
inputs and the outputs thus far.  So I decided to include input and output in
the `Program` type, and not use my State Monad implementation.

Including these in the type that tracks the state, along with a "Relative Base"
which the Instruction Pointer is taken as being relative to, we get:

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

As every odd day from 5 onwards uses this intcode computer, I defined it in
it's own module in a different directory and wrote some tests for it.

Looking around I found all sorts of testing libraries with different goals:
[hUnit][hunit] for unit testing, [SmallCheck][smallcheck] &
[QuickCheck][quickcheck] for property based testing, to name a few.  The
[tasty][tasty] library attempts to pull them all together so I went with that.

Property based testing looks quite interesting and I plan to look into it.  For
my intcode I added a bunch of `assert F(X) == Y` style tests to ensure the
operations perform as expected in the mainline cases.

With [tasty][tasty] you create a "test tree".  I added all the examples from
explanations of the intcode features from each of the days that introduces
them.  The output looks quite good in the terminal.

```
$ stack test
...
intcode> test (suite: intcode-test)

Tests
  Unit tests
    Test progressing the program by one instruction
      tests for op codes 1 & 2
        Day 2 example 1 step 1:                              OK
        Day 2 example 1 step 2:                              OK
        Day 2 example 1 step 3 - finish:                     OK
        Day 2 example 1 - try step finished prog:            OK
      Some testing of Opcodes 5 & 6
        op code 6 - test 1:                                  OK
      test 203 op code
        simple case:                                         OK
    Test runnning the program til it stops
      Test examples from day 2 of AOC
        Day 2 example 2 - (1 + 1 = 2):                       OK
        Day 2 example 3 (3 * 2 = 6):                         OK
        Day 2 example 4 (99 * 99 = 9801):                    OK
        Day 2 example 5:                                     OK
      Test examples from day 5 of AOC
        Day 5 example 1 - echo:                              OK
        Day 5 example 2 - immediate mode:                    OK
        Day 5 example 3 - input equal to 8? yes:             OK
        Day 5 example 3 - input equal to 8? no:              OK
        ... (etc)
      Test examples from day 9 of AOC
        Day 9 example 1 - copy of self:                      OK
        Day 9 example 2 - output 16 digit number:            OK
        ... (etc)

All 33 tests passed (0.01s)

intcode> Test suite intcode-test passed
Completed 2 action(s).
```

## Using the Intcode Computer

[Day 7][day7] asks you to create a series of intcode computers, each running an
"amplifier program" provided, where the output of one becomes the input of the
next.

The first part of the problem requires you to run these in series to get an
output at the end.  _Taking the ascii diagrams from the [day 7][day7] problem
description:_

```
    O-------O  O-------O  O-------O  O-------O  O-------O
0 ->| Amp A |->| Amp B |->| Amp C |->| Amp D |->| Amp E |-> (to thrusters)
    O-------O  O-------O  O-------O  O-------O  O-------O
```

A simple fold over a list of intcodes sufficed here.  I was happy for each
intcode state to be thrown away as the calculation moved to the next.  No need
for the State Monad yet!

The second half of the problem then asks you to run the intcode computers, the
"amplifiers", in a loop, until they exit.

```
      O-------O  O-------O  O-------O  O-------O  O-------O
0 -+->| Amp A |->| Amp B |->| Amp C |->| Amp D |->| Amp E |-.
   |  O-------O  O-------O  O-------O  O-------O  O-------O |
   |                                                        |
   '--------------------------------------------------------+
                                                            |
                                                            v
                                                     (to thrusters)
```

Now we care about keeping the intcode state around, and we have an output we
want to take from the stateful computation, we have a good reason to use the
State Monad.

```haskell
import Intcode (
  Status,
  Program
)

data Amps = Amps {
  ampsOf::[Program],
  activeAmp::Int
} deriving(Show)

runAmpsLoop :: State Amps (Maybe Int)
runAmpsLoop = do
  out <- runActiveAmpOutput'
  amps <- get
  case (atFinalAmp amps, activeAmpStatus amps) of
    (_, Crashed) -> return Nothing
    (_, Running) -> return Nothing
    (True, Terminated) -> return (Just out)
    (False, Terminated) -> do
      modify nextAmp
      modify (activeAmpAppendInput out)
      runAmpsLoop
    (_, AwaitInput) -> do
      modify nextAmp
      modify (activeAmpAppendInput out)
      runAmpsLoop

--
-- The types of the functions called above,
-- in order of appearence.
--
runActiveAmpOutput' :: State Amps Int
runActiveAmpOutput :: Amps -> (Int, Amps)

atFinalAmp :: Amps -> Bool
activeAmpStatus :: Amps -> ProgState

nextAmp :: Amps -> Amps
activeAmpAppendInput :: Int -> Amps -> Amps
```

Following the same pattern applied when I tried wrapping the intcode in a State
Monad [above][intcodestate]:
- `runActiveAmpOutput` evolves the state and provides an output, our `s -> (a, s)`;
- `runActiveAmpOutput'` wraps that in the State Monad with `state`; and
- `runAmpsLoop`:
  + runs the current amplifier (intcode program),
  + gets the new overall state with `get`, and
  + only if we've just run the final amplifier in the loop, and it has `Terminated`,
    return the output.
  + Otherwise, if we haven't crashed, use `modify` to edit the overall state by
    moving to the next amplifier in the loop, `nextAmp`, and passing the output
    from the last amplifier to new one, `activeAmpAppendInput`.

I also defined a function `newAmps` which, given an intcode program and a list
of "phases" (tuning parameters for the amplifiers), produces the initial `Amps`
state.

```haskell
newAmps :: [Int] -> [Int] -> Amps
```

I could then find the output of running the amplifiers in a loop with the
expression:

```haskell
evalState runAmpsLoop $ newAmps intCode phases
```

The problem itself asks you to find the combination of phases that gives the
greatest output once the amplifier loop has ran its course.  This could be
found by selecting the permutation of phases that the above expression evaluates
to the greatest value.

## Introducing Monad Transformers

[Day 13][day13] has a fun problem.  You're given an intcode program that outputs
the data (triplets of: x coordinate, y coordinate, and a "tile id" that determines
which character exists at that position) for the display of a game where you
destroy all the bricks with a ball that you keep in play with a paddle you move
side to side.

You're asked to write a program that will finish the game by destroying all the
bricks.

I had a solution that would start with a state like this:

```
"Z|||||||||||||||||||||||||||||||||||||||||||"
" | # # ## #### ##  #  #### #####    #####  |"
" | ####    #   #   # #    ## # #   ##   #  |"
" | # ## ## # ## ## ###  ######  # ##### #  |"
" | #  # ## ####    ##### # ######  ######  |"
" | ##  ##### # # #  ## # # ### ## ### ## # |"
" |  #### ##  #  # ## #  #   ## # ###### #  |"
" |  ## #######  #### # ## # #   ## #  #### |"
" | # # ## #### ##  #  #### #####    #####  |"
" | # ##### #   ##    ### ####    # ####  # |"
" |  ## #  #   #    #   # #  ## # # #  ###  |"
" | ## ## # # # ##  #    ## ###    ##  #### |"
" | ###     ######   ## ##  ## ### ##       |"
" |    # ## ##   ##  ## ### #         ## ## |"
" | ## ##  #  # ### # ## #   ##    ### # ## |"
" |                                         |"
" |                                         |"
" |                         o               |"
" |                                         |"
" |                          =              |"
" |                                         |"
```

and would whirr away for a short while until it was finished and my `main`
could print the game state:

```
"Z|||||||||||||||||||||||||||||||||||||||||||"
" |                                         |"
" |                                         |"
" |                                   o     |"
" |                                         |"
" |                                         |"
" |                                         |"
" |                                         |"
" |                                         |"
" |                                         |"
" |                                         |"
" |                                         |"
" |                                         |"
" |                                         |"
" |                                         |"
" |                                         |"
" |                                         |"
" |                                         |"
" |                                         |"
" |                                   =     |"
" |                                         |"
"your score: 13581"
```

To achieve this I followed the same pattern as I did [in the day 7
solution][usingintcode] we saw in the last section:

* Define the state,
* Wrap that in the State Monad, and
* Define a recursive function to keep running the intcode
  computer and evolving the state until done.

```haskell
import qualified Intcode as IC
import qualified Data.HashMap.Strict as HM

data Game = Game {
  gameProg :: IC.Program,
  gameDisplay :: Displ
} deriving(Show)

type Displ = HM.HashMap Tile Tid
type Tile = (Int, Int)

data Tid = Empty | Wall | Block | Paddle | Ball | BadTid | Score{theScore::Int} deriving(Show, Eq)
```

This time `Game` is the state, containing an intcode program and a
[HashMap][hashmap] to record the "tiles" in the display, keyed by their
coordinates.

```haskell
data LiveData = LiveData {
  -- x-coordinate of the ball - xb
  xb::Int,
  -- x-coordinate of the paddle - xp
  xp::Int,
  -- Game score - sc
  sc::Int
} deriving(Show)

stepGame' :: [Int] -> Game -> (LiveData, Game)

stepGame :: [Int] -> State Game LiveData
stepGame = state . stepGame'

playGame :: [Int] -> State Game Int
playGame inp = do
  liveData <- stepGame inp
  game <- get
  case IC.progState (gameProg game) of
    IC.AwaitInput -> playGame [joystick (xb liveData) (xp liveData)]
    _ -> return $ sc liveData

joystick :: Int -> Int -> Int
```

`LiveData` is the data we pass on from step to step: the x coordinates of the
ball and paddle and the running score.  The intcode program given can take `1`,
`-1`, or `0` as an input to move the paddle right, left, or nowhere. `joystick`
returns `1`, `-1`, or `0` depending on whether the ball is ahead, behind, or in
line with the paddle.

This was all well and good, and I could complete the problem by giving my end
score, but I wanted to watch my program as it took out all the bricks.

Basically I wanted to be able to call `print` inside the `playGame` function.
I had a function that would turn the HashMap display into a 2D grid of characters:

```haskell
gridDisplay :: Displ -> [[Char]]
```

, and could print a game's display to screen with an expressions like:

```haskell
mapM print $ gridDisplay displ
```

that maps `print` across a `[[Char]]` (i.e. `[String]`) to display the
grid in `stdout`.

To be able to perform an IO action within `playGame` we need to be able
to include a line of code that returns the IO Monad inside our State Monad.

To achieve "Monads inside Monads" we use [monad transformers][monadtrans],
and for the State Monad we can use the [`StateT`][statet] monad transformer.

Editing the `playGame` function in my solution:

```haskell
import Control.Monad.State.Lazy (state, StateT)
import Control.Monad.IO.Class (liftIO)
import System.Console.ANSI (cursorUp)

playGame :: [Int] -> StateT Game IO Int
playGame inp = do
  liveData <- stepGame inp
  game <- get

  --
  -- Display the game state in a grid to stdout
  --
  let grid = gridDisplay $ gameDisplay game
  _ <- liftIO $ mapM print grid
  liftIO . print $ ("Your score: " ++ show (sc liveData))
  liftIO . cursorUp $ (length grid) + 1

  case IC.progState (gameProg game) of
    IC.AwaitInput -> playGame [joystick (xb liveData) (xp liveData)]
    _ -> return $ sc liveData

stepGame :: [Int] -> StateT Game IO LiveData
stepGame = state . stepGame'
```

As `State` is only a type alias for `StateT` applied to the [Identity Monad][idm], I've really
changed the type of `playGame` from:

```haskell
playGame :: [Int] -> StateT Game Identity Int
```

to:

```haskell
playGame :: [Int] -> StateT Game IO Int
```

to provide a non-trivial "inner monad".

The other key ingredients here are: `liftIO`, which allows us to perform
actions within the inner IO Monad; and `cursorUp`, to move the stdout cursor up
so we refresh the screen rather than printing a new grid below.

While I got a monad transformer to work for me in this instance, I need to go
and spend a bit more time to understand how they work in general, hence the
reasonable brief explanation here.

The result of this is that I could sit and watch my program destroy all the
bricks! :tada:

```
"Z|||||||||||||||||||||||||||||||||||||||||||"
" |                                         |"
" | ####    #   #   # #    #  # #   ##   #  |"
" | # ## ## # ## ## ###  ##   #  # ##### #  |"
" | #  # ## ###     ##### #    ###  ######  |"
" | ##  ##### #   #  ## # # #   #  ### ## # |"
" |  #### ##  #    ## #  #         ##### #  |"
" |  ## #######     # # ##          #  #### |"
" | # # ## ####        ###   #       #####  |"
" | # ##### #         ### # #         ##    |"
" |  ## #  #   #        # #  #         ###  |"
" | ## ## # # # #        ## ###         #   |"
" | ###     ######       #  ## #            |"
" |    # #  ##            #                 |"
" | ## ##     # #      #     ##             |"
" |                                         |"
" |                                      o  |"
" |                                         |"
" |                                         |"
" |                                     =   |"
" |                                         |"
"Your score: 3939"
```

```
"Z|||||||||||||||||||||||||||||||||||||||||||"
" |                                         |"
" | ##                               #   #  |"
" | # #       ##                    #### #  |"
" | #  # #    #                # #  ######  |"
" | ##  ##    #                 #  ### ## # |"
" |  #### ##       #                 ### #  |"
" |  ## #             # #              #### |"
" | # #      #         ###             ###  |"
" | # #               ### #         o ##    |"
" |  #                  # #            # #  |"
" | #                    ## ###             |"
" | ##                   #  ##              |"
" |                       #                 |"
" |                                         |"
" |                                         |"
" |                                         |"
" |                                         |"
" |                                         |"
" |                                  =      |"
" |                                         |"
"Your score: 9021"
```

It does get a bit boring watch it spend ages trying to get
the last few bricks.

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

This pattern showed a lot of promise, and I wanted to see if I could employ
it in my intcode computer code.

### Refactoring the Intcode Computer

Recall the `Program` type I've been using to model an intcode computer
thus far:

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

We think about where failure is represented in this type: in the `Crashed`
variant of the sum type `Status` under the status field.  You could say that
"sense of failure" is buried deep within this type.  If we want to handle
failure with Monads, we want to encode failure in a "container" type.

I chose to refactor this by making what was `Status` into a type that
had variants that could contain an intcode computer. The data from the `Program`
type above, then got split into:

```haskell
data Prog a = Running a | AwaitInput a | End a | Crashed String deriving(Show, Eq)

data Intcode = Intcode {
  input::[Int],
  code::[Int],
  -- ip: Instruction Pointer
  ip::Int,
  -- rb: Relative Base
  rb::Int,
  output::[Int]
} deriving(Show, Eq)
```

With `Prog` being given implementations of `Monad` and `MonadFail` typeclasses:

```haskell
instance Monad Prog where
  return = Running
  (Crashed e) >>= _ = (Crashed e)
  (End prog) >>= k = k prog
  (Running prog) >>= k = k prog
  (AwaitInput prog) >>= k = k prog

instance MonadFail Prog where
  fail = Crashed
```

As the definition of the [monad typeclass][monadtypeclass] has the typeclass constraint:

```haskell
class Applicative m => Monad m where
...
```

and `Applicative` in turn has a similar constraint for `Functor`, I needed to also
make `Prog` an instance of both the `Applicative` and `Functor` typeclasses:

```haskell
instance Functor Prog where
  fmap _ (Crashed e) = Crashed e
  fmap f (End a) =  End (f a)
  fmap f (Running a) = Running (f a)
  fmap f (AwaitInput a) = AwaitInput (f a)

instance Applicative Prog where
  pure = Running
  _ <*> (Crashed e) = Crashed e
  (End f) <*> prog = fmap f prog
  (Running f) <*> prog = fmap f prog
  (AwaitInput f) <*> prog = fmap f prog
```

So what was previously represented by `Program`, is now represented by `Prog Intcode`,
where `Prog` is a monad that can represent a program being in a number of states, or
crashed and `Intcode` is the data relevant to an intcode program specifically.  The
abstraction here is to separate the raw number crunching of the intcode computer from
our interpretation of the overall "status" of the program.

Having made this monad, I refactored all the operations on the intcode which could fail
to return a type generic across `MonadFail`, rather than returning `Maybe`.  E.g. my
function to safely lookup an index in a list and return `0` if the index is beyond
the end of the list, `(!!!)` went from:

```haskell
(!!!) :: Integral a => [a] -> Int -> Maybe a
infixl 9 !!!
xs !!! i
  | i < 0 = Nothing
  | i >= length xs = Just 0
  | otherwise = Just $ xs !! i
```

to:

```haskell
(!!!) :: Integral a => MonadFail m => [a] -> Int -> m a
infixl 9 !!!
xs !!! i
  | i < 0 = fail $ "index " ++ show i ++ " less that zero"
  | i >= length xs = return 0
  | otherwise = return $ xs !! i
```

I was then able to strip out all of the `case` statements that were matching
`Maybe` and returning a "crashed" program for `Nothing` and use `do` notation.

The implementation of the old `runStep` function, that moved the program on by
running a single instruction, called down through layers of functions to select
the right operation, try to perform it, matching on the `Maybe` returned and
evolving the intcode state if the operation was successful.  Now this logic
is condensed with the tedious boilerplate removed.  The start of the equivalent
function in the refactored implementation looks like:

```haskell
runInstruction :: Intcode -> Prog Intcode
runInstruction ic = do
   opcodes <- currentOpCode ic
   case opcodes of
      (One, m1, m2, m3) -> do
        newIc <- op1 m1 m2 m3 (ip ic) (code ic)
        return $ moveIp 4 . updateCode newIc $ ic
   ...
```

`currentOpCode` & `op1` both return a type generic over `MonadFail`, so in the
case of failure will cause this function to bail with the `Crashed` variant of
`Prog`.  There is one of these 3 line blocks for each of the different instructions
the intcode can have, and that's it.

I'm not sure how "idiomatic" using `MonadFail` in this way is, it's just
something I spotted that I could do. I see that the [`Data.List.Safe`][safelist]
uses `MonadThrow` to represent failure, and looking around there seems
other ways of representing and dealing with errors, the [`ExceptT`][exceptt]
monad transformer among them.

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
article][opinionatedguide], by adding this to my `~/.stack/config.yaml`:

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
of: knowing the data I want; thinking procedurally about all the steps
between that and the data I have; writing a function for each of these
steps; and stringing them together to get the answer.

This gets me to the answer, but the result is a load of free floating
functions non of which make any real sense on their own.  The code ends
up being a bit of a mess, especially when those constituent steps are
not used anywhere else.

Part of this can come down to whether either of the data I have or the
data I want are actually good representations of the problem.  When data
is a good natural fit to a problem you tend not to need to do so many
complex transformations of that data.  But I think it's fair to expect
times when you need to describe the transformation of some data that's
more complex than could be described in a one line function.

I started using the `where` keyword more often, and that appears to
have dealt with this problem I was having.  With `where` you can break
down a calculation into parts without exposing those parts as free
floating functions.

E.g:

```haskell
foo = map . bar baz
  where
    bar = ...
    baz = ...
```

This keeps the readers attention in the block of text where the function
is defined, rather than having to jump around a file, or even into different
files.

## Modules & Namespacing

In [my last post][part1] I talked about: not knowing how to find out
what package to install to get a certain library; and not being totally
comfortable with the namespacing when modules are imported and suffering
from "where did that function come from?" syndrome.

The first of these was me just being dumb as it turns out the
package containing a module is written in the top left hand corner
of the module documentation's web page.  Spot `mtl-` in [the
docs for `Control.Monad.State.Lazy`][docsstate].

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

As I said last time, I reckon I could press on and do the rest of the problems
with the tools I know.  But I'm keen to level up my Haskell and not just grind
out solutions.

I'm keen to read more into monad transformers and property based testing.

I then want to loop back around and improve the performance of my solutions:
looking into strictness and more efficient data structures.  I also want to
make types fit the data better: where they could be more generic making them
more generic and where they're too generic, and possible values of the type
don't represent possible values of data the type represents, choose, or define,
another type that's better suited.

_As with [Part 1][part1], all my solutions are [mastered on Github][tarquinaoc]._

[monadfail]: http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad-Fail.html
[tarquinaoc]: https://github.com/tarquin-the-brave/aoc-19-haskell
[valueofvalues]: https://www.youtube.com/watch?v=-6BsiVyC1kM
[simplemadeeasy]: https://www.youtube.com/watch?v=oytL881p-nQ
[opinionatedguide]: https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/
[part1]: https://tarquin-the-brave.github.io/blog/posts/re-learning-haskell/
[idepost]: https://tarquin-the-brave.github.io/blog/posts/ide-read-code/
[day2]: https://adventofcode.com/2019/day/2
[day5]: https://adventofcode.com/2019/day/5
[day7]: https://adventofcode.com/2019/day/7
[day9]: https://adventofcode.com/2019/day/9
[day13]: https://adventofcode.com/2019/day/13
[docsstate]: https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Lazy.html
[wikistate]: https://wiki.haskell.org/State_Monad
[lyahstate]: http://learnyouahaskell.com/for-a-few-monads-more#state
[safelist]: http://hackage.haskell.org/package/listsafe-0.1.0.1/docs/Data-List-Safe.html
[maybe]: http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Maybe.html
[dryblog]: https://tarquin-the-brave.github.io/blog/posts/dry-not-a-goal/
[smallcheck]: https://hackage.haskell.org/package/smallcheck
[quickcheck]: http://hackage.haskell.org/package/QuickCheck
[hunit]: https://hackage.haskell.org/package/HUnit
[tasty]: https://hackage.haskell.org/package/tasty
[intcodestate]: #introducing-the-state-monad
[hashmap]: https://hackage.haskell.org/package/unordered-containers-0.2.10.0/docs/Data-HashMap-Strict.html
[usingintcode]: #using-the-intcode-computer
[monadtrans]: https://wiki.haskell.org/Monad_Transformers
[statet]: https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Lazy.html#t:StateT
[idm]: https://hackage.haskell.org/package/base-4.11.0.0/docs/Data-Functor-Identity.html#t:Identity
[monadtypeclass]: https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad.html#t:Monad
[exceptt]: http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Except.html#t:ExceptT


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
