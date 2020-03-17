+++
title = "Expressivity and Control aren't Separable"
date = 2020-03-17T19:06:53Z
images = []
tags = []
categories = []
draft = true
+++

Recently I've found myself again in the position of creating some developer tooling
and getting into a familiar discussion about how much "power" to grant the user vs.
how much "control" to exert.

This conversation usually ends up resolving with the statement that there is a trade
off between expressivity and control, and that immediate expressivity is powerful and
controlled expression is somehow more limited in power.

This argument has never sat that well with me.
what about type systems? abstraction with confidence?

is bash powerful?  or are functions powerful?

# My Recent Example

Helm is a good example of very expressive, but not powerful. String templating,
control flow, low testability and refactorability.

# Footguns

# Control is Part of Expressivity

So the point I've been trying to get to, is that we can't have a tool that's
ultimately expressive and powerful without the layers of control that allow
us to make abstractions with confidence.

It's not meaningful to talk about the expressivity of something if there isn't
the control sufficient for us to reason about and trust the elements we're
abstracting over.

So when we assess the expressivity and power of something, we can't just consider
the size of the domain of things we can write down with it, but whether we can
make abstractions with it confidently.  When are we most powerful: when we're standing
on the shoulders of giants, or jelly?
