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
patterns that a C developer might employ to attempt to write memory safe and
thread safe code.

Regardless of the language we're writing in, these data manipulation patterns
are there. The language may formalise them, it may not. By formalising data
manipulation patterns in a language, there's the opportunity for the compiler
to tell you if you're using them right, otherwise you're performing these
patterns by hand.

What it comes down to is:
[![](https://mermaid.ink/img/eyJjb2RlIjoiZ3JhcGggVERcbiAgQVtEb2VzIHlvdXIgbGFuZ3VhZ2UgZm9ybWFsaXNlIHRoZXNlIHBhdHRlcm5zP11cbiAgQyhbVGhlIGNvbXBpbGVyIGtlZXBzIHlvdSByaWdodCBmYTpmYS10aHVtYnMtdXBdKVxuICBCW0FyZSB5b3UgYXdhcmUgeW91J3JlIHdyaXRpbmcgdGhlbSBieSBoYW5kP11cbiAgRChbWW91J2xsIHdyaXRlIG1vcmUgY29uc2lzdGVudCwgY29ycmVjdCwgYW5kIG1haW50aW5hYmxlIGNvZGUgZmE6ZmEtdGh1bWJzLXVwXSlcbiAgRShbQnVncyB3aWxsIGhhcHBlbiBmYTpmYS1idWddKVxuICBBIC0tPnxZZXN8IENcbiAgQSAtLT58Tm98IEJcbiAgQiAtLT58WWVzfCBEXG4gIEIgLS0-IHxOb3wgRVxuXHRcdCIsIm1lcm1haWQiOnsidGhlbWUiOiJkZWZhdWx0IiwidGhlbWVWYXJpYWJsZXMiOnsiYmFja2dyb3VuZCI6IndoaXRlIiwicHJpbWFyeUNvbG9yIjoiI0VDRUNGRiIsInNlY29uZGFyeUNvbG9yIjoiI2ZmZmZkZSIsInRlcnRpYXJ5Q29sb3IiOiJoc2woODAsIDEwMCUsIDk2LjI3NDUwOTgwMzklKSIsInByaW1hcnlCb3JkZXJDb2xvciI6ImhzbCgyNDAsIDYwJSwgODYuMjc0NTA5ODAzOSUpIiwic2Vjb25kYXJ5Qm9yZGVyQ29sb3IiOiJoc2woNjAsIDYwJSwgODMuNTI5NDExNzY0NyUpIiwidGVydGlhcnlCb3JkZXJDb2xvciI6ImhzbCg4MCwgNjAlLCA4Ni4yNzQ1MDk4MDM5JSkiLCJwcmltYXJ5VGV4dENvbG9yIjoiIzEzMTMwMCIsInNlY29uZGFyeVRleHRDb2xvciI6IiMwMDAwMjEiLCJ0ZXJ0aWFyeVRleHRDb2xvciI6InJnYig5LjUwMDAwMDAwMDEsIDkuNTAwMDAwMDAwMSwgOS41MDAwMDAwMDAxKSIsImxpbmVDb2xvciI6IiMzMzMzMzMiLCJ0ZXh0Q29sb3IiOiIjMzMzIiwibWFpbkJrZyI6IiNFQ0VDRkYiLCJzZWNvbmRCa2ciOiIjZmZmZmRlIiwiYm9yZGVyMSI6IiM5MzcwREIiLCJib3JkZXIyIjoiI2FhYWEzMyIsImFycm93aGVhZENvbG9yIjoiIzMzMzMzMyIsImZvbnRGYW1pbHkiOiJcInRyZWJ1Y2hldCBtc1wiLCB2ZXJkYW5hLCBhcmlhbCIsImZvbnRTaXplIjoiMTZweCIsImxhYmVsQmFja2dyb3VuZCI6IiNlOGU4ZTgiLCJub2RlQmtnIjoiI0VDRUNGRiIsIm5vZGVCb3JkZXIiOiIjOTM3MERCIiwiY2x1c3RlckJrZyI6IiNmZmZmZGUiLCJjbHVzdGVyQm9yZGVyIjoiI2FhYWEzMyIsImRlZmF1bHRMaW5rQ29sb3IiOiIjMzMzMzMzIiwidGl0bGVDb2xvciI6IiMzMzMiLCJlZGdlTGFiZWxCYWNrZ3JvdW5kIjoiI2U4ZThlOCIsImFjdG9yQm9yZGVyIjoiaHNsKDI1OS42MjYxNjgyMjQzLCA1OS43NzY1MzYzMTI4JSwgODcuOTAxOTYwNzg0MyUpIiwiYWN0b3JCa2ciOiIjRUNFQ0ZGIiwiYWN0b3JUZXh0Q29sb3IiOiJibGFjayIsImFjdG9yTGluZUNvbG9yIjoiZ3JleSIsInNpZ25hbENvbG9yIjoiIzMzMyIsInNpZ25hbFRleHRDb2xvciI6IiMzMzMiLCJsYWJlbEJveEJrZ0NvbG9yIjoiI0VDRUNGRiIsImxhYmVsQm94Qm9yZGVyQ29sb3IiOiJoc2woMjU5LjYyNjE2ODIyNDMsIDU5Ljc3NjUzNjMxMjglLCA4Ny45MDE5NjA3ODQzJSkiLCJsYWJlbFRleHRDb2xvciI6ImJsYWNrIiwibG9vcFRleHRDb2xvciI6ImJsYWNrIiwibm90ZUJvcmRlckNvbG9yIjoiI2FhYWEzMyIsIm5vdGVCa2dDb2xvciI6IiNmZmY1YWQiLCJub3RlVGV4dENvbG9yIjoiYmxhY2siLCJhY3RpdmF0aW9uQm9yZGVyQ29sb3IiOiIjNjY2IiwiYWN0aXZhdGlvbkJrZ0NvbG9yIjoiI2Y0ZjRmNCIsInNlcXVlbmNlTnVtYmVyQ29sb3IiOiJ3aGl0ZSIsInNlY3Rpb25Ca2dDb2xvciI6InJnYmEoMTAyLCAxMDIsIDI1NSwgMC40OSkiLCJhbHRTZWN0aW9uQmtnQ29sb3IiOiJ3aGl0ZSIsInNlY3Rpb25Ca2dDb2xvcjIiOiIjZmZmNDAwIiwidGFza0JvcmRlckNvbG9yIjoiIzUzNGZiYyIsInRhc2tCa2dDb2xvciI6IiM4YTkwZGQiLCJ0YXNrVGV4dExpZ2h0Q29sb3IiOiJ3aGl0ZSIsInRhc2tUZXh0Q29sb3IiOiJ3aGl0ZSIsInRhc2tUZXh0RGFya0NvbG9yIjoiYmxhY2siLCJ0YXNrVGV4dE91dHNpZGVDb2xvciI6ImJsYWNrIiwidGFza1RleHRDbGlja2FibGVDb2xvciI6IiMwMDMxNjMiLCJhY3RpdmVUYXNrQm9yZGVyQ29sb3IiOiIjNTM0ZmJjIiwiYWN0aXZlVGFza0JrZ0NvbG9yIjoiI2JmYzdmZiIsImdyaWRDb2xvciI6ImxpZ2h0Z3JleSIsImRvbmVUYXNrQmtnQ29sb3IiOiJsaWdodGdyZXkiLCJkb25lVGFza0JvcmRlckNvbG9yIjoiZ3JleSIsImNyaXRCb3JkZXJDb2xvciI6IiNmZjg4ODgiLCJjcml0QmtnQ29sb3IiOiJyZWQiLCJ0b2RheUxpbmVDb2xvciI6InJlZCIsImxhYmVsQ29sb3IiOiJibGFjayIsImVycm9yQmtnQ29sb3IiOiIjNTUyMjIyIiwiZXJyb3JUZXh0Q29sb3IiOiIjNTUyMjIyIiwiY2xhc3NUZXh0IjoiIzEzMTMwMCIsImZpbGxUeXBlMCI6IiNFQ0VDRkYiLCJmaWxsVHlwZTEiOiIjZmZmZmRlIiwiZmlsbFR5cGUyIjoiaHNsKDMwNCwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwiZmlsbFR5cGUzIjoiaHNsKDEyNCwgMTAwJSwgOTMuNTI5NDExNzY0NyUpIiwiZmlsbFR5cGU0IjoiaHNsKDE3NiwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwiZmlsbFR5cGU1IjoiaHNsKC00LCAxMDAlLCA5My41Mjk0MTE3NjQ3JSkiLCJmaWxsVHlwZTYiOiJoc2woOCwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwiZmlsbFR5cGU3IjoiaHNsKDE4OCwgMTAwJSwgOTMuNTI5NDExNzY0NyUpIn19LCJ1cGRhdGVFZGl0b3IiOmZhbHNlfQ)](https://mermaid-js.github.io/mermaid-live-editor/#/edit/eyJjb2RlIjoiZ3JhcGggVERcbiAgQVtEb2VzIHlvdXIgbGFuZ3VhZ2UgZm9ybWFsaXNlIHRoZXNlIHBhdHRlcm5zP11cbiAgQyhbVGhlIGNvbXBpbGVyIGtlZXBzIHlvdSByaWdodCBmYTpmYS10aHVtYnMtdXBdKVxuICBCW0FyZSB5b3UgYXdhcmUgeW91J3JlIHdyaXRpbmcgdGhlbSBieSBoYW5kP11cbiAgRChbWW91J2xsIHdyaXRlIG1vcmUgY29uc2lzdGVudCwgY29ycmVjdCwgYW5kIG1haW50aW5hYmxlIGNvZGUgZmE6ZmEtdGh1bWJzLXVwXSlcbiAgRShbQnVncyB3aWxsIGhhcHBlbiBmYTpmYS1idWddKVxuICBBIC0tPnxZZXN8IENcbiAgQSAtLT58Tm98IEJcbiAgQiAtLT58WWVzfCBEXG4gIEIgLS0-IHxOb3wgRVxuXHRcdCIsIm1lcm1haWQiOnsidGhlbWUiOiJkZWZhdWx0IiwidGhlbWVWYXJpYWJsZXMiOnsiYmFja2dyb3VuZCI6IndoaXRlIiwicHJpbWFyeUNvbG9yIjoiI0VDRUNGRiIsInNlY29uZGFyeUNvbG9yIjoiI2ZmZmZkZSIsInRlcnRpYXJ5Q29sb3IiOiJoc2woODAsIDEwMCUsIDk2LjI3NDUwOTgwMzklKSIsInByaW1hcnlCb3JkZXJDb2xvciI6ImhzbCgyNDAsIDYwJSwgODYuMjc0NTA5ODAzOSUpIiwic2Vjb25kYXJ5Qm9yZGVyQ29sb3IiOiJoc2woNjAsIDYwJSwgODMuNTI5NDExNzY0NyUpIiwidGVydGlhcnlCb3JkZXJDb2xvciI6ImhzbCg4MCwgNjAlLCA4Ni4yNzQ1MDk4MDM5JSkiLCJwcmltYXJ5VGV4dENvbG9yIjoiIzEzMTMwMCIsInNlY29uZGFyeVRleHRDb2xvciI6IiMwMDAwMjEiLCJ0ZXJ0aWFyeVRleHRDb2xvciI6InJnYig5LjUwMDAwMDAwMDEsIDkuNTAwMDAwMDAwMSwgOS41MDAwMDAwMDAxKSIsImxpbmVDb2xvciI6IiMzMzMzMzMiLCJ0ZXh0Q29sb3IiOiIjMzMzIiwibWFpbkJrZyI6IiNFQ0VDRkYiLCJzZWNvbmRCa2ciOiIjZmZmZmRlIiwiYm9yZGVyMSI6IiM5MzcwREIiLCJib3JkZXIyIjoiI2FhYWEzMyIsImFycm93aGVhZENvbG9yIjoiIzMzMzMzMyIsImZvbnRGYW1pbHkiOiJcInRyZWJ1Y2hldCBtc1wiLCB2ZXJkYW5hLCBhcmlhbCIsImZvbnRTaXplIjoiMTZweCIsImxhYmVsQmFja2dyb3VuZCI6IiNlOGU4ZTgiLCJub2RlQmtnIjoiI0VDRUNGRiIsIm5vZGVCb3JkZXIiOiIjOTM3MERCIiwiY2x1c3RlckJrZyI6IiNmZmZmZGUiLCJjbHVzdGVyQm9yZGVyIjoiI2FhYWEzMyIsImRlZmF1bHRMaW5rQ29sb3IiOiIjMzMzMzMzIiwidGl0bGVDb2xvciI6IiMzMzMiLCJlZGdlTGFiZWxCYWNrZ3JvdW5kIjoiI2U4ZThlOCIsImFjdG9yQm9yZGVyIjoiaHNsKDI1OS42MjYxNjgyMjQzLCA1OS43NzY1MzYzMTI4JSwgODcuOTAxOTYwNzg0MyUpIiwiYWN0b3JCa2ciOiIjRUNFQ0ZGIiwiYWN0b3JUZXh0Q29sb3IiOiJibGFjayIsImFjdG9yTGluZUNvbG9yIjoiZ3JleSIsInNpZ25hbENvbG9yIjoiIzMzMyIsInNpZ25hbFRleHRDb2xvciI6IiMzMzMiLCJsYWJlbEJveEJrZ0NvbG9yIjoiI0VDRUNGRiIsImxhYmVsQm94Qm9yZGVyQ29sb3IiOiJoc2woMjU5LjYyNjE2ODIyNDMsIDU5Ljc3NjUzNjMxMjglLCA4Ny45MDE5NjA3ODQzJSkiLCJsYWJlbFRleHRDb2xvciI6ImJsYWNrIiwibG9vcFRleHRDb2xvciI6ImJsYWNrIiwibm90ZUJvcmRlckNvbG9yIjoiI2FhYWEzMyIsIm5vdGVCa2dDb2xvciI6IiNmZmY1YWQiLCJub3RlVGV4dENvbG9yIjoiYmxhY2siLCJhY3RpdmF0aW9uQm9yZGVyQ29sb3IiOiIjNjY2IiwiYWN0aXZhdGlvbkJrZ0NvbG9yIjoiI2Y0ZjRmNCIsInNlcXVlbmNlTnVtYmVyQ29sb3IiOiJ3aGl0ZSIsInNlY3Rpb25Ca2dDb2xvciI6InJnYmEoMTAyLCAxMDIsIDI1NSwgMC40OSkiLCJhbHRTZWN0aW9uQmtnQ29sb3IiOiJ3aGl0ZSIsInNlY3Rpb25Ca2dDb2xvcjIiOiIjZmZmNDAwIiwidGFza0JvcmRlckNvbG9yIjoiIzUzNGZiYyIsInRhc2tCa2dDb2xvciI6IiM4YTkwZGQiLCJ0YXNrVGV4dExpZ2h0Q29sb3IiOiJ3aGl0ZSIsInRhc2tUZXh0Q29sb3IiOiJ3aGl0ZSIsInRhc2tUZXh0RGFya0NvbG9yIjoiYmxhY2siLCJ0YXNrVGV4dE91dHNpZGVDb2xvciI6ImJsYWNrIiwidGFza1RleHRDbGlja2FibGVDb2xvciI6IiMwMDMxNjMiLCJhY3RpdmVUYXNrQm9yZGVyQ29sb3IiOiIjNTM0ZmJjIiwiYWN0aXZlVGFza0JrZ0NvbG9yIjoiI2JmYzdmZiIsImdyaWRDb2xvciI6ImxpZ2h0Z3JleSIsImRvbmVUYXNrQmtnQ29sb3IiOiJsaWdodGdyZXkiLCJkb25lVGFza0JvcmRlckNvbG9yIjoiZ3JleSIsImNyaXRCb3JkZXJDb2xvciI6IiNmZjg4ODgiLCJjcml0QmtnQ29sb3IiOiJyZWQiLCJ0b2RheUxpbmVDb2xvciI6InJlZCIsImxhYmVsQ29sb3IiOiJibGFjayIsImVycm9yQmtnQ29sb3IiOiIjNTUyMjIyIiwiZXJyb3JUZXh0Q29sb3IiOiIjNTUyMjIyIiwiY2xhc3NUZXh0IjoiIzEzMTMwMCIsImZpbGxUeXBlMCI6IiNFQ0VDRkYiLCJmaWxsVHlwZTEiOiIjZmZmZmRlIiwiZmlsbFR5cGUyIjoiaHNsKDMwNCwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwiZmlsbFR5cGUzIjoiaHNsKDEyNCwgMTAwJSwgOTMuNTI5NDExNzY0NyUpIiwiZmlsbFR5cGU0IjoiaHNsKDE3NiwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwiZmlsbFR5cGU1IjoiaHNsKC00LCAxMDAlLCA5My41Mjk0MTE3NjQ3JSkiLCJmaWxsVHlwZTYiOiJoc2woOCwgMTAwJSwgOTYuMjc0NTA5ODAzOSUpIiwiZmlsbFR5cGU3IjoiaHNsKDE4OCwgMTAwJSwgOTMuNTI5NDExNzY0NyUpIn19LCJ1cGRhdGVFZGl0b3IiOmZhbHNlfQ)

When I first learned some Haskell a few years ago, the idea of spending my free
time doing that was sold to me as "it'll make you better at every other
language".  That was enough to sell it to me at the time, and I believe it had
that effect.  It's true of every language to some extent.  Every language
formalises some number of data manipulation patterns, the question is: to what
extent? And are they good patterns to be using to get correct and maintainable
code?

There is the argument I've seen where someone might answer "No" to both of the
above, adding "and it doesn't matter". I've seen claims that learning the
concepts that formalise patterns of data manipulation is overly esoteric and
not pragmatic.  To try to understand these things makes you a "purist", and
gets in the way of "getting things done".  I think the reality is that
software, in general, has had growth as a higher priority than quality and
correctness for some amount of time. A lot of code can have quite a short shelf
life so preference is given to being able to produce code quickly and the
negative side effects of that aren't felt so much. I'm not totally convinced by
this position. There's potential for a feedback loop where the consequences of
trying to produce things quickly drive the short shelf lives.  And I'm not
entirely convinced that languages that do formalize a lot of these patterns and
have a compiler that keeps you right on them is generally slower to produce
things.  There's learning time that you have to put in, but once you've learned
to satiate the compiler on your first try, you don't tend to spend that much
time fighting it.  There are specific problems that are going to be slower
develop solutions to, but I don't thing it's necessarily true generally.  There
a counter argument that to produce anything of a meaningful size you can
produce things faster because the compiler has stopped you hitting things that
you would hit and have to address before you were able to ship something.  This
one probably depends on the problem you're trying to build a solution to.

Regardless of the relative strengths of those arguments, it's certainly true
that developers have been able to "get away with" not having these patterns
formalised and not necessarily knowing they're hand rolling them. Whether that
continues into the future is going to be quite interesting.

As software becomes more prevalent in all industries, every professional is
writing code, and every child learns coding in school (which they already are):
I'd wager we'll see the general quality bar expected by users/consumers to go
up, as they'll have greater visibility into how unnecessary a lot of broken and
slow software is. As the systems we're building are getting more interconnected
and complex, is correctness going to become more of a priority?

This isn't to say that there won't always be bugs in software.  No matter how
solid the tools you use are, you could have just missed the mark on
requirements and built the wrong thing.  But there are whole classes of bugs
and errors that can be eliminated by formalising these patterns and encoding
them into the fundamental tools with which we build things.

I work with Rust most working days.  While it's missing higher kinded types,
it's got a lot going for it with its strong type system, its ownership model,
and generally how it makes you be explicit about what you're doing.  People say
with Rust that "if it compiles it works", and provided you've written code that
leverages the type system, there's some truth to that.  I might re-phrase that
to "if it compiles then what you've asked for is guaranteed to be consistent in
various ways, but you might still have asked for the wrong thing", but that's a
little less catchy. It makes me hopeful for the future, and I believe that Rust
will stand as one of the cornerstones of a new generation of software that
works.

[aoc]: https://adventofcode.com/2019/
[part1]: https://tarquin-the-brave.github.io/blog/posts/re-learning-haskell/
[part2]: https://tarquin-the-brave.github.io/blog/posts/re-learning-haskell-2/
