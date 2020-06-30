+++
title = "DRY Shouldn't be a Goal in Itself"
date = 2020-04-18T10:40:01+01:00
images = []
tags = []
categories = []
draft = false
+++

[DRY][dry], "Don't Repeat Yourself", is often described as a "principle" of
software engineering.  I looked up what "principle" is defined to mean and
Google says:

> a fundamental truth or proposition that serves as the foundation
  for a system of belief or behaviour or for a chain of reasoning.

Maybe in this case that's a bit strong.  I'd think in the case of a software
principle we're talking more about an idea of how things should be, or a
guide[^guidelines] to decision making.

But anyway.  I find quite often, when seeing discussion about software, DRY
ends up being front and centre, the main focus of what someone wants to
achieve.  People argue for DRY "at any cost".  Is it really worth eliminating
every ounce of repetition if the result is completely unreadable and
unmaintainable code?  An answer to that could take us down the route of "DRY
done right", "DRY done wrong", and I'm sure there will be endless articles out
there that are titled as such.  But I find that whenever you see discussion
around "principle X done right", "principle X done wrong", that serves as an
indicator that there's something quite leaky about the principle. :fountain:

I think it's better to think of DRY as an indicator that abstractions are good,
rather than something to shoot for in of itself.

Like in Economics, "full employment" can be a sign that an economy is generally
healthy, but can't be a goal above all else.  We could employ everyone in the
postal service: we'd have full employment, but no food.  I only did Economics
up to GCSE, so maybe there's more to the story than that, but you get the
picture.

As stupid as this comparison might be, I see this happening in software.  DRY
seems to be an obsession for some developers, and then once achieved: "the
answer".

I wonder why this is.  Perhaps because it's easy, visible, and safe.  No-one
disagrees in general that less repetition is better, so unless they know the
specifics of what you're talking about they're not going to disagree with you.

I've seen the pursuit of DRY in absolute leave a trail of destruction in its
wake.  I prefer to think of it as an indicator, or early warning signal for my
abstractions.  If I see a fair amount of repetition in my code, I question if
my abstractions are quite right.  If I seem to be able to express quite a lot
with very little code, and not a huge number of functions and modules, I see it
as good sign that the abstractions are about right.

I could take verbose and repetitive code, and chop it up into loads of
functions and squirrel them away in modules "utils\_1", "utils\_2", &
"utils\_3", and I would have "achieved DRY".  But the code would be more all
over the place than when I started and I wouldn't have addressed the underlying
cause of the problem: that the abstractions were just wrong.

> Dry can be a sign of good abstractions, but not a goal in itself.

So work on your abstractions, and the repetition will fall away as you get them
right.

[dry]: https://en.wikipedia.org/wiki/Don%27t_repeat_yourself

[^guidelines]: As Captain Barbossa says: "The code is more what you'd
               call guidelines than actual rules".
