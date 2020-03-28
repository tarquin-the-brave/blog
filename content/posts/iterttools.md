+++
title = "Itertools - A Force for Good?"
date = 2020-03-28T12:52:30Z
images = []
tags = []
categories = []
draft = false
+++

Itertools are a way of bringing along a bunch of list manipulating goodies from
functional languages into procedural languages.  I've encountered it as a
[Python library][itertools-py] and a [Rust crate][itertools-rs].  A quick bit
of google'ing shows some evidence of this existing in other languages.

I used to write Python a lot.  Now I write mostly Rust.  I used to err on the
side of not using `itertools` in Python.  I'm more than happy to read or write
Rust code using `itertools`.  The reasons why are about audience buy in, how
effectively are you communicating an idea, and ultimately what effect does it
have on people's ability to reason about your code.

For a Rust developer, coming across the [Itertools crate][itertools-rs] doesn't
require a big leap of understanding. The `Itertools` trait is merely an
extension to the [`Iterator` trait][iter-rs] from the standard library which
has methods for the likes of `map`, `fold`, `filter`, `chain`, etc.  These
concepts from functional languages are already first class citizens in Rust.
You don't need the reader to cross that conceptual bridge of "what's wrong with
a for loop?", they're already across with you.  Rust is explicit about how
values are passed to functions (by reference/by value, mutably/immutably), and
imposes borrowing rules.  This means you can iterate with confidence.  Also,
Rust is strongly typed and has fantastic documentation as a core tenet of the
language.  This mean that even if there are some concepts introduced that you
don't quite understand, or you don't quite follow an example of a method from
the documentation: you can trust the type, and the compiler will tell you if
you've misunderstood.  If you're reading code, and your not all that familiar
with the concept behind a method, you can can still reason about how data is
being manipulated by reading the type declaration of the function.

In Python, on the other hand, using [`itertools`][itertools-py] and
[`functools`][functools-py] introduces functional concepts[^1], which are a
whole new dimension to the language. The nature of the language (duck-typed,
implicit about how values are passed, and runtime evaluated) doesn't give the
same safety net and leg up to those not already very familiar with the concepts
at play and the pitfalls of their implementation in Python.  It'll depend a
little on what the particular community who you share a codebase with are like,
but there's likely to be a reasonable proportion of Python developers who don't
necessarily know what you've done.  For them: you've taken something that could
have been expressed in a few lines, and tucked it away behind a word they
haven't seen before.  And then when they do learn what that method is doing,
it's not like they can abstract over that forever more.

Functional languages can have all this wonderful functionality abstracted away
into single words (`fold`, `intercalate`, ...) because data is immutable and
functions don't have side effects.  Once you've learned how a particular
function works, you can let it do its thing.  Maybe you pop back to glance at
the type signature (if it has one) from time to time, but mostly you know: it
does a thing, you can abstract over it with confidence.


Rust does quite a special thing of allowing mutation, but provide a mechanism
where that mutation is explicit and the type system provides enough confidence
that you can make abstractions safely.

An example that a friend sent me recently, which isn't specifically about
`itertools` or `functools` but is an example of where these lower level details
that are hidden from the syntax really can catch you out.  Lambda
functions create anonymous functions in-line, and are equivalent to
defining a function elsewhere and calling it.

```
$ python3
Python 3.6.9 (default, Nov  7 2019, 10:44:02)
[GCC 8.3.0] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> x = 4
>>> x
4
>>> x = lambda: 4
>>> x
<function <lambda> at 0x7f6b8665abf8>
>>> x()
4
>>>
```

Assign an anonymous function to `x` and then call it.  Happy with that.  Last
week, a friend of mine sent me this:

```python
[x() for x in [lambda: i for i in range(0,3)]]
```

Let's see what that does:

```
>>> [x() for x in [lambda: i for i in range(0,3)]]
[2, 2, 2]
```

Wat?  Let's try building up to that:

```
>>> [i for i in range(0,3)]
[0, 1, 2]
>>> [x for x in [i for i in range(0,3)]]
[0, 1, 2]
>>> [x for x in [lambda: i for i in range(0,3)]]
[<function <listcomp>.<lambda> at 0x7f6b8665ad90>, <function <listcomp>.<lambda> at 0x7f6b8665ae18>, <function <listcomp>.<lambda> at 0x7f6b8665aea0>]
>>> [x() for x in [lambda: i for i in range(0,3)]]
[2, 2, 2]
>>>
```

Wat?

When I worked in Python a lot I would see this sort of thing happen all the
time.  Either you couldn't understand why your code wasn't doing the simple
thing you were expressing, or worse, committed code was silently doing the
wrong thing causing an error at the opposite end of the application.  To figure
out what's going on you have to context switch out of the "higher level" world
you've been happily reading and writing code in, remind yourself about how each
thing works behind the scenes and attempt to reason it through.

This is why I think Python is a bit self defeating.  To be great at Python, to
read Python and understand what it's really doing, to write Python that works
and is performant, you need an understanding of what's going on behind the
scenes. But the whole point of Python is that it's a higher level language.
The main beauty of it for me, is that someone who doesn't know any programming
languages can read it and have a decent guess at what's going on.  It's code in
plain English.  But to get good you need to learn about what's going on behind
the scenes.  Python isn't going to teach you those things.  You'd need to learn
those things in another, supposedly "lower level", language and bring those
learnings back into your Python.

Because you have to keep these lower level details in your mind when working
with Python, with mutability and how values are passed being implicit, you
can't abstract with confidence.  These functional goodies from `itertools` and
`functools` lose their power and run the risk of becoming more "complexity
hiding" than good abstractions.

Were I to go back to writing Python regularly I would probably look to make use
of `itertools` & `functools` as I reckon I'd probably write more bugs if I were
to write it out in loops manually.  But I'd only do it with buy in from
everyone I was working on that Python project with, and I'd keep some notes
around about what the methods I'm using are doing under the covers for the time
when I inevitably trip up.

So are "Itertools" a force for good in the world.  Yes, provided the language
can provide the guarantees that make the abstractions safe and powerful.

[^1]: A [blog post][functional-python-blog] that gives a quick run through of using
      functional concepts in Python.  It's much more approachable
      than the [official docs][functional-python-docs] I like how the author presents
      it as "here are some things you _can_ do, I'm not saying you
      _should_ use any of them".  And they cover how functional
      patterns are considered "not Pythonic" by some... Not that
      someone waving a magic word like _Pythonic_ in your face should
      ever stop you from doing anything.

[iter-rs]: https://doc.rust-lang.org/std/iter/trait.Iterator.html
[itertools-py]: https://docs.python.org/3.8/library/itertools.html
[itertools-rs]: https://docs.rs/itertools/0.9.0/itertools/trait.Itertools.html
[functools-py]: https://docs.python.org/3/library/functools.html
[functional-python-docs]: https://docs.python.org/3/howto/functional.html

[functional-python-blog]: https://skerritt.blog/learn-functional-python-in-10-minutes/
