+++
title = "Is it time to Stop Referring to things as Functional?"
date = 2020-08-27T12:29:17+01:00
images = []
tags = []
categories = []
draft = false
+++

Language is transient.  It evolves through a cascade of misappropriations.
"Correct" usage is only ever as strongly defined as "the current generally
accepted usage".

There's many words that in the past meant something quite different[^meaning],
"silly" used to mean "blessed". Current common misappropriations of words are
the beginning of that process. Even if by today's reckoning, everything Alanis
Morissette claims to be "ironic"[^ironic] is instead just "unfortunate", one
day that might be what "ironic" is taken to mean... Literally!

Now consider the word "function".  To a mathematician, or a programmer talking
in the context of what we know as "functional languages", it's a referentially
transparent description of how some data maps from some other data.  To a
programmer thinking in a procedural paradigm (OOP included), it's a
sub-routine.

This difference in understanding of what a "function" is causes a chasm between
programmers.  Whether we like it or not, the software community has rather
strongly misappropriated the word "function" to the extent that it's taken on a
different meaning.

This then makes it less meaningful to refer to concepts or languages as
"functional".  With the majority of programmers not having the same
understanding of what a "function" is as the understanding that underpins
"functional" being a meaningful description, the description of "functional"
loses its meaning and value.

If not "functional", what should we call it?  I quite like "data oriented".
Rich Hickey does a great job[^val] of explaining the difference between "data"
and "places".  "Data" representing observed facts about the world, and "places"
being those bits of computer memory that we use to temporarily store those
facts or other things.  Hickey coins the term "PLOP" (PLace Oriented
Programming) to describe languages where the programmer is, knowingly or not,
making statements about the places in memory that the program will use to
process its data.

"Functional" languages aren't PLOP.  You don't tell the program to create a
location in memory, put data in, change that data (mutation), etc.  In a
"functional" language you describe data.  You describe how the data you want
maps from the data you have with "functions".

If instead of describing things as "functional" we described them as "data
oriented", I wonder if that would help with programmers being introduced to the
concepts and distinguish them from what they already know. I think it would
also help to the value and simplicity of a "functional"/"data oriented"
approach. You describe how data flows through your program.  You don't need to
worry about all the moving parts that PLOP brings that can ruin your
abstractions at any moment.

> Instead of "functional", should we say "data oriented"?

But then what should we call "functions"?

[val]: https://www.youtube.com/watch?v=-6BsiVyC1kM
[meaning]: https://www.bbc.co.uk/bitesize/articles/znbct39
[ironic]: https://www.google.com/search?q=alanis+morissette+ironic+lyrics

[^val]: Rich Hickey talk: ["The Value of Values"][val]
[^meaning]: BBC article on: ["Five words that have changed meaning over time"][meaning]
[^ironic]: I can't see anything in [the lyrics][ironic] to "Ironic" that's actually ironic.
           "Rain on your wedding day".  If you'd gone to the effort of arranging a wedding
           in the Atacama desert, specifically to avoid being rained on, then yes, it's
           ironic.  But in places where it rains, weddings get rained on.
