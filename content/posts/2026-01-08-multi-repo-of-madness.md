+++
title = "The Multi-Repo of Madness"
date = 2026-01-08T20:41:06Z
images = []
tags = ["architecture", "devops"]
categories = []
draft = false
aliases = ["/posts/multi-repo-of-madness/"]
+++

I should confess upfront: I meant to write this post about four years ago and
never got round to it.  The tide has turned somewhat towards monorepos now,
especially with AI-assisted coding making "grep the whole codebase" more
valuable than ever.  So this cautionary tale about going multi-repo is perhaps
less relevant than it once was.

But still.

---

A long time ago I worked at a company that traditionally built big monolithic
products.  Telecoms infrastructure, massive C servers, that sort of thing.

They decided the next generation of products would be different: microservices,
containers, Kubernetes.  Everyone would share a common language (that's how I
ended up working with Rust), share libraries and frameworks, and compose
services in various ways onto VMs built from common base images.

So the setup was: many teams, sharing common libraries, composing microservices
in different configurations to make different products, all running on shared
infrastructure.  Multi-repo felt quite natural for this.  People seem to want
repos to have clear team ownership.  People like small, clean repos.  Harder to
break abstractions when there's a repo boundary in the way.  A divergence from
the monolithic past.

[![](https://mermaid.ink/img/pako:eNp9VE2P2yAU_CsIycolWcWA7Y0PPWzbw0rNpVvtoXUPxCYJkm0ijN2mUf57AcdfOFkuhjczvHl-wAWmImMwhp534SVXMbgs1JEVbBGDRclqJWm-uF49Lyn3ufiTHqlU4MdLUgI9qnp3kPR0BDnfVb8S-L2uFPjGd5JKzqoE_m5pZmiGrxm5Bc--i6EBQy6GBwz3GCuzpHRcVEw2PGXGyZanUnTryYZVkxojN8x3MTRgyMXwgGEXIwNGXCwYsODDAni5l1STX823UrJOVS0ZeLtXh-WaStrJHEMdhj7M2RTmf71vwWtBD06OHa1YU2jYTFZNseKGM6EcpKhPlnOSItOGVzZyn3yjTOhNcc-e5w3nCJiDp2tSou9wSzJnCqxWn2xP-xCeh1AXQnMhmgvxXIjnQjJnBSP7XdNG7m8lg_ftrQfao5X2_6WPowdx7MbdfMTmq9qEpm3jbMSq267OpcFYars41gZWe2v3SPzS5rinHcfc2lsT0z1H5-lhmc7VeFRseykm5Q4XYxKGS3iQPIOx3pMtYcFkQc0SXowkgfYxTGCspxnb0zpXCUzKq5adaPlTiKJTauOHI4z3NK_0qj5lVLEvnOpbVjjRrxlXQnY6Wivxdi7Tfh99DZj8LOpSwRitfZsIxhf4F8Y-Ik8EhxijaI1CHEZoCc-aFW6eogjjKCQheSY4Cq5L-M96Wz89--twQ_AGRcFGa6IlZDb_tn337fN__Q-qeeDT)](https://mermaid.live/edit#pako:eNp9VE2P2yAU_CsIycolWcWA7Y0PPWzbw0rNpVvtoXUPxCYJkm0ijN2mUf57AcdfOFkuhjczvHl-wAWmImMwhp534SVXMbgs1JEVbBGDRclqJWm-uF49Lyn3ufiTHqlU4MdLUgI9qnp3kPR0BDnfVb8S-L2uFPjGd5JKzqoE_m5pZmiGrxm5Bc--i6EBQy6GBwz3GCuzpHRcVEw2PGXGyZanUnTryYZVkxojN8x3MTRgyMXwgGEXIwNGXCwYsODDAni5l1STX823UrJOVS0ZeLtXh-WaStrJHEMdhj7M2RTmf71vwWtBD06OHa1YU2jYTFZNseKGM6EcpKhPlnOSItOGVzZyn3yjTOhNcc-e5w3nCJiDp2tSou9wSzJnCqxWn2xP-xCeh1AXQnMhmgvxXIjnQjJnBSP7XdNG7m8lg_ftrQfao5X2_6WPowdx7MbdfMTmq9qEpm3jbMSq267OpcFYars41gZWe2v3SPzS5rinHcfc2lsT0z1H5-lhmc7VeFRseykm5Q4XYxKGS3iQPIOx3pMtYcFkQc0SXowkgfYxTGCspxnb0zpXCUzKq5adaPlTiKJTauOHI4z3NK_0qj5lVLEvnOpbVjjRrxlXQnY6Wivxdi7Tfh99DZj8LOpSwRitfZsIxhf4F8Y-Ik8EhxijaI1CHEZoCc-aFW6eogjjKCQheSY4Cq5L-M96Wz89--twQ_AGRcFGa6IlZDb_tn337fN__Q-qeeDT)

---

## Where It Went Wrong

We went too far with it.

### You Couldn't Test Everything Locally

There was no easy way to locally test patches of dependencies all the way up
the stack.  It was manageable enough with the Rust repos - you can use path
dependencies and such - but once you hit the microservice boundary, where
things shipped in containers, you'd have to build and publish a version of the
container to test anything.

You could imagine the scenario: you've got a fix for a library, and you want to
see if it actually solves the problem in the service that uses it.  In a
monorepo you'd just... run the service locally with your change.  In our setup,
you'd have to push your branch, wait for CI to build, publish a dev version of
the crate, update the service's Cargo.toml, push that, wait for its CI to
build a container, then finally deploy that container somewhere you could poke
at it.  By which point you've forgotten what you were testing.

### Releases Took Days

Libraries had to be released, then microservices, then VM images.  Especially
painful when the change was in a component baked into the VM infrastructure.
Long chains of "internal versioned releases" just to ship something to
customers.

I was in a group of three teams working on the same product, and each fortnight
someone had to volunteer as tribute to spend the best part of a week
shepherding the release through.  This wasn't an exaggeration.  You'd spend
Monday kicking off the library releases.  Tuesday you'd be chasing down why
some microservice CI had failed.  Wednesday you'd be debugging why the VM build
was stuck.  Thursday you'd realise someone had merged something that broke the
integration tests.  Friday you'd start over.

We tried automating it, but with CI pipelines deploying and live-testing VMs on
a flaky private OpenStack cloud, it could never be reliable enough to not need
human intervention.  Someone always had to be there to notice the failure, work
out if it was a real problem or just flakiness, and decide whether to retry or
investigate.

### Some Things Could Only Be Tested on Real VMs

On the VM infrastructure side we had separate repos for binaries that ran in
systemd units on the product VMs.  These couldn't be meaningfully tested
outside of an actual VM.  The binaries interacted with system services, storage
mounts, network configuration - all stuff that didn't exist in a unit test
environment.

It was an endless loop of release candidate builds, or hacking patches onto
live VMs just to test something.  SSH in, copy the binary over, restart the
service, tail the logs, realise you got something wrong, repeat.  It worked,
but it wasn't exactly confidence-inspiring.

### DRY Made Things Worse

There was lots of repeated boilerplate between repos.  Every repo needed CI
configuration, Docker builds, release workflows, the same linting setup.  So we
tried to fix this the obvious way: common "CI templates" that repos could pull
in like libraries.

It was horrible.  The templates became these fragile, over-parameterised
monstrosities.  Every repo had slightly different needs, so the templates grew
conditionals and options until nobody understood them.  When something broke,
you'd be debugging YAML inheritance across multiple repos.  DRY isn't always
the answer.

### You Didn't Know What You Didn't Know

And then there was the problem of not knowing about some repo hidden away
somewhere that you might not even have access to.  You'd be debugging an issue,
following the trail, and suddenly hit a wall because the relevant code was in a
repo you'd never heard of, owned by a team you'd never spoken to.

We ended up building something hacky for "cross-repo search" because often the
best tool to find the source of an issue is just: grep.  If you can't grep it,
you can't debug it.

---

## The Rule I Came Up With

> Don't break up repos beyond the boundary of what you can meaningfully test
> independently.

If something needs to be released and pulled into another repo before you can
test it to an acceptable level, it shouldn't be in its own repo.

It's a bit heuristic, but it make sense in my head at least.

---

## Where I Landed

My opinion on multi-repo got jaded from this experience.  I started out excited
about small, single-purpose, easy-to-understand repos.  I ended up hating the
complexity of the web of how they all fit together.

But I still don't love monorepos either.  They're big, CI can be slow, and it's
easier to break abstractions when everything's right there.

The main thing that bothers me is that crappy code and tech debt can propagate
inside them.  With a multi-repo setup you often "start fresh" when you spin up
a new service - you write something that's up to date with everything you've
learned from previous projects.  In a monorepo it's easier to perpetuate the
patterns in the code around you.  You see how the adjacent service does
something, you copy it, and now you've got two services doing it the old way
instead of one.

This is especially true now that LLMs are writing a lot of code.  They're
excellent at pattern-matching against what's already in the repo.  If your repo
is full of old patterns, they'll happily reproduce them.  The tech debt becomes
self-perpetuating.

If I had to choose right now, I'd probably go monorepo.  It's easily
grep-able.

But really the "multi vs mono" question isn't that important.  They both have
their own special ways of being painful.  What matters is your tooling.  You
can have a monorepo where it's quick and easy to edit, build, test, and deploy
single components or multiple components at once - CI only running for the bits
you changed.  And you can have a multi-repo setup where the repo boundaries fit
how things are actually deployed, tested, and depend on each other at runtime.
That's another reason I lean monorepo these days: picking those boundaries well
is hard.

---

As I said, this is somewhat out of date now.  But it's a true tale of when
"going full multi-repo" wasn't everything it was cracked up to be.

The real message: it's the tooling that matters, not the choice of mono vs
multi.

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
this blog likely out of date now. I meant to write it 4 years ago and didn't get round to it.  it's
supposed to be cautionary tale about going multi-repo, not the tide is turning towards monorepos
to assist AI driven coding this is less relevant.

but still.

A long long time ago. I worked at a company that traditionally had big mono-repo products,
telecoms infrastructure, monolithic servers in C.

they decided the next phase of products will all be microservice, container, kubernetes based, share
a common language (that's how I ended up working with Rust), share libraries, frameworks, etc.  composed
together in various ways onto VMs who's images were based of common base images to make different products.

so: many teams, sharing common libraries, composing microservices in different ways to make products
using common infrastructure.  Multi-repo felt quite natural: people seem to want repos to have definite
team ownership, peole like small clean repos, makes it harder to break abstractions, divergence from the past.

(placeholder - diagram will go here)

But we went too far with it. flaws:
- no easy way to locally test patches of dependencies all the way up the stack.  easy enough with rust repos
  but when we got to the microservice boundary, with then shipped in containers, we'd have to build
  and ship a version of the container.
- so many layers that the release took forever - days. libraries, microservices, VMs. Especially when
  the change was in a component added into the VM infra.  Long chain of "internal versioned releases"
  to do a release of the products. I was in a group of 3 teams that worked on the same product and
  each fortnight someone from one of the teams had to volunteer as tribute to spend the best part of a
  week sheparding the release.  We tried automating this but with CI pipelines deploying and live testing
  VMs on a flaky private cloud on OpenStack, it could never be reliable enough to not need human intervention.
- not so bad for libraries in to services, unless they did significant I/O, but on the VM infra side we
  had separate repos for binaries that would be run in systemd units on the product VMs which could not
  be meaningfully tested outside of the VM.  Endless loop of release candidate releases or hacking patches
  onto live VMs to test something.
- lots of repeated boilerplate between repos, as much as we tried to have common libraries etc.
  Our attempt to have common "CI templates" that repos could use as if they were libraries was horrible.
  DRY isn't always the answer.
- not knowing about a certain repo hidden away somewhere that you might not have access to.
- we ended up doing something hack for "cross repo search" - as often the best tool to find the source
  of an issue is: grep

rule I came up with was: don't break up repos beyond the boundary of what you can meaningfully test
independently.  If something needs to be released and pulled down into another repo to test to an
acceptable level, it shouldn't be in it's own repo.


Certainly my opinion on multi-repo got jaded from this experience.  started excited about small single
purpose, easy to understand repos, ended hating the complexity of the web of how they all fit together.

But I still don't like monorepos: big, can have slow build CI, easier to break abstractions.  Actually the
main reason I don't like monorepos is that crappy code and techdebt can propogate inside them.  With more
of a multi-repo setup you often "start from fresh" and write something fresh that is up to date with
everything you've learned from how things went in previous projects.  In a monorepo its easier to
perpetuate the patterns in code you have around you.  this is especially true now LLMs are writing a lot
of code.  they will see the precedent of the current patterns in the repo and copy them, dragging the
tech debt in them forward with it.

if I had to chose, i'd probably go with monorepo right now - it's easily grep'able.

But really the "multi vs mono repo" question is not that important.  They both suck in their own special
ways. It's really about your tooling.  You can have a monorepo where it's quick and easy to edit,
build, test, deploy single components or multiple components at the same time, e.g. CI only running for
the relevant stuff you changed.  And it's possible to have a multi-repo setup that has repo boundaries
that fit how things are deployed, tested, and their runtime dependencies.  That's another reason I'd
prefer monorepos as picking those boundaries can be hard.

As I said, this article is somewhat out of date these days anyway. (i made notes to write an article on
this about 3 years ago).  but it's a true tale of when "going full multi-repo" wasn't everything it was
cracked up to be.

really the message is: it's the tooling that matters, not the choice of mono vs multi.
```

</details>
