+++
title = "Expressivity and Control aren't Separable"
date = 2020-03-17T19:06:53Z
images = []
tags = []
categories = []
draft = true
+++

Recently I've found myself again in the position of creating some developer
tooling and getting into a familiar discussion about how much "power" to grant
the user vs.  how much "control" to exert.

This conversation usually ends up resolving with the statement that there is a
trade off between expressivity and control, and that immediate expressivity is
powerful and controlled expression is somehow more limited in power.

This argument has never sat that well with me.  what about type systems?
abstraction with confidence?

is bash powerful?  or are functions powerful?

# My Recent Example

Helm is a good example of very expressive, but not powerful. String templating,
control flow, low testability and refactorability.

# Footguns

# An Analogy

You're building a race car. You can put as big engine in as you like, but if
you haven't got the chassis, brakes, suspension, differentials, downforce,
you're not going to get round the track fast.  You might not even get round the
first corner.

If you consider "the speed of the car" as being its lap times, which is the end
goal of building a racing car, all those systems of control are vital to the
lap times and are a key parts of the speed.

It wouldn't make sense to talk about how quick the car is round the track,
without those aspects of control.

# Control is _Part of_ Expressivity

So the point I've been trying to get to, is that we can't have a tool that's
ultimately expressive and powerful without the layers of control that allow us
to make abstractions with confidence.

It's not meaningful to talk about the expressivity of something if there isn't
the control sufficient for us to reason about and trust the elements we're
abstracting over.

So when we assess the expressivity and power of something, we can't just
consider the size of the domain of things we can write down with it, but
whether we can make abstractions with it confidently.  When are we most
powerful: when we're standing on the shoulders of giants, or jelly?

# Extending This Idea

I vaguely recall watching a talk about rust performance recently where there
were figures of it beating C at certain tasks and the presenter was proposing
that rust's correctness provided by the compiler's strong type system and
borrow checker was having a positive effect on how performant rust programs
were.  I'll have to dig that out, and do some more research, before making any
grand statements about how this idea extends to program performance. I
suspect that control and guarantees of correctness also contribute to
performance.  Our [race car analogy](#an-analogy) might be reflective of
reality in that case as well, at least for real world programs where humans can
drive them into the gravel pit if they don't have enough control.

