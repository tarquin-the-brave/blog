+++
title = "Rust Perspectives for Experienced Engineers"
date = 2026-01-09T11:11:59Z
images = []
tags = ["rust"]
categories = []
draft = false
+++

About a year ago I joined a company.  They'd brought me in as "someone with
lots of Rust experience".  From the interviews I thought they'd have a few
projects here and there in Rust.  Turns out it was almost the entire stack for
their new product.  A lot of experienced folks from other languages were
learning Rust for the first time while pushing to ship.

Watching how people write Rust when coming from other languages, I thought
about what perspectives might be useful to share.  Ways of seeing things that
make Rust click a bit easier.

---

## It's Data Oriented, Not Object Oriented

You don't need "objects".  Rust has structs and enums and implementations, but
the mental model of objects with encapsulated state and methods that mutate
that state... you can mostly let go of that.

Errors are just data too.  They deserve the same thought and care as your
happy-path types.  I've written about this [elsewhere](/posts/no_errors_in_rust/).

## Everything Is a Value

It's all about data.  Values flow through your program.  This supports writing
very direct code - you can often just follow the data and understand what's
happening.

## Think About Stack and Heap

In many languages you don't really think about where your data lives.  In Rust
it matters.  Getting comfortable with what goes on the stack versus what gets
boxed onto the heap will help you reason about ownership and lifetimes.

## Everything Is Off by Default

Rust doesn't give you things you didn't ask for.  Traits aren't implemented
unless you derive or impl them.  Fields are private by default.

To be fair, I think private-by-default for struct fields was a mistake.  It
implies these are objects with invariants to protect, when often they're just
data.  But that's how it is.

The principle though: don't turn things on for no reason.  If you don't need
`Clone`, don't derive it.  If you don't need `pub`, don't add it.

## Code Is Text

Turn off your IDE for a bit.  Well, not completely - but try reading Rust
without leaning on go-to-definition and type hints everywhere.

Yes, it's more explicit.  Yes, it's more verbose.  But there's very little
hidden magic.  What you see is mostly what you get.  That explicitness is the
point.

## There Is No One "Rusty" Way

Don't let anyone tell you there's a single correct way to do something in Rust.
There are usually about ten ways to do anything, and they're all valid.
Different trade-offs, different contexts.

The community can be a bit prescriptive sometimes.  Take it with a pinch of
salt.

## Watch Out for Blocking I/O in Async

Rust doesn't protect you here.  If you call a blocking function inside an async
context, you'll block the executor and bad things happen.  The compiler won't
warn you.

## It Punishes Premature Abstraction

Rust's strong type system means if you make naive abstractions early on, the
language will hold you to them.  When those abstractions turn out to be
unsound, life gets hard.  You'll be fighting the compiler instead of working
with it.

Better to start concrete and extract abstractions when you actually need them.

## It's Going to Be Frustrating Sometimes

The ambitious goals of the language have come with compromises.  It's not
perfect.

Ease of use is probably 90% familiarity.  The other 10% is subjective and
depends on what time horizon you're thinking about - some things are easy at
first and hard later, others the reverse.

The compiler is your friend.  Take the time to read and understand the errors.
Don't just do what it suggests straight away - you could end up going round in
circles.  Understand *why* it's complaining first.

These aren't rules, just perspectives that helped me.  Rust rewards patience
and a willingness to see things a bit differently.

---

<details>
<summary><i>A note on how this post was written</i></summary>

_Disclaimer: I hadn't posted in this blog for many years mostly because I didn't
get round to it.  I had ideas for blog posts, but didn't find the time to write
them out.  For a while I also was doing technical blogging on the website
for the company I worked for, so got my "technical writing fix" from that.  Despite
that, I've had years at a time where I didn't write any technical blog, which
I regret, as I think it is good for the mind and good for the soul.  To kickstart
me getting some of my thoughts down on paper again, I'm experimenting with using
Claude code to assist me.  I've tried developing a "skill" by getting it to process
all my previous blog and all my writings in Slack to try to capture my "voice".
I'm giving it a stream of conciousness notes and asking it to write that into prose
in my voice.  So these posts are "written by AI", but only with ideas I've told it
to write, and hopefully in my voice. I'll include at the bottom my raw notes that
this article is derived from._

**Raw notes this post was derived from:**

```
about 1 year ago I joined a company. brought in a "someone with lots of rust experience".
from interview I thought that they would only have a few projects here and
there in Rust, actually it was almost all of the stack for their new product...
a lot of experienced folks from other languages learning rust for the first time as they
were pushing to ship a new product.

seeing how people write rust coming from other languages, I thought what
perspectives would be useful to point out to help see things from a perspective
that makes rust easier.

Rust perspectives:

- Data oriented, not object oriented
    - "objects" are not needed.
    - Errors are just data - worth the same thought (see adjacent blog post).
- Everything is a value → it's all about data. Data flow supports direct code.
- Stack and heap
- Everything off by default - don't turn it on for no reason
    - Implementations
    - Privacy - except struct fields - IMO that was a mistake for struct fields to be private by default - it implies objects, when it's just data.
- Code is text - turn off your ide (yes explicitly, yes verbose, but no hidden magic - mostly)
- Don't let anyone tell you the "rusty" way of doing something. There's like 10 ways to do anything and they're all valid.
- Watch out for blocking I/O in async. Rust doesn't protect you here. And I can only apologise for this.
- It'll punish you for premature abstraction. Strong type system means if you make naive abstractions, the language will hold you to them, and make life hard when they turn out to be unsound.
- It's going to be frustrating sometimes.
    - Ambitious goals of the language have come with compromises. It's not perfect.
    - Ease of use is 90% familiarity. Other 10% is subjective on time frame, easy at first, hard later or vice versa.
    - compiler is your friend.  Take the time to read and understand the errors.  Don't just do what it suggests straight away - you could go round in circles.
```

</details>
