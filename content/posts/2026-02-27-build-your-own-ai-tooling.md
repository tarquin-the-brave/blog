+++
title = "Build Your Own AI Tooling"
date = 2026-02-27T10:58:25Z
images = []
tags = []
categories = []
draft = false
+++

The experience of adopting AI agents, Claude Code in my case, appears to
be one of:

Install Claude Code.  Use it directly.  This works OK for small, one-off,
tasks.  But then you hit a limit on the complexity it can deal with, it runs out
of context, you have to repeatedly remind it to do and not do things a certain
way.

In steps "Context Engineering" and all manner of layers of `CLAUDE.md` files,
skills, hooks, and frameworks to orchestrate "sub agents" and "agent teams".
These allow you to not have to manually manage the context window, encode
behaviours that you want repeated (or not repeated), ultimately allowing it
to take on larger, more complex tasks without constant supervision.

And you can install these off of the internet.  You're just one `npm install`
away from having your Claude behave in a different way - maybe it's even the
way you wanted it to behave.  [Claude Code Templates][cct] and [Context
Engineering Kit][cek] being examples of rich repositories of downloadable
doohickies.

Of course there's a security problem with this.  Like installing anything off
the internet to run on your machine (it's extraordinary how many projects' install
instructions are `curl ... | sh`...), it's very worth reading the files that
were installed before you run them.  And it's easier to do so with these things
as they generally comprise markdown files and the odd script in Python or JS.

You can pretty quickly install stuff, and away you go.

---

I did this.  It seemed to work quite nicely for a bit.  Then at some point
there's something you don't like about the way it's working, so you ask Claude
to edit the skill.  Although I found things never worked as reliably as I'd
hoped, and as I'd just installed them off internet I didn't really know how
or why they were getting, or not getting, the results they purport to.

The fact that you can "ask claude to edit the skill" presents an alternative
approach of "work with claude to develop a skill or framework", "test it and
iteratively improve it yourself".

After all, how do you think all those installable hooks and skills and
frameworks that you can install got made?  My bet would be people working with
their AI agent to generate them, and as soon as it sort of appears to be doing
something useful - share it.  This is precisely what I did with the skill I
shared in [my last post][mlp].  I tested and refined it for a couple of hours.
Once I was content it was more useful than nothing, I shared it.

Then after a while all the things you've installed build up, start eating your
context window, and you need to go through and clear things out.

Also new model versions arrive, and the foundations these layers are built on,
the behaviours that they are there to corral, the shortcomings they are there
to address, change.  I've seen this first-hand this year - when Claude Code's
"plan mode" got an upgrade, I found myself uninstalling frameworks for planning
out work.

Also I watched a good [youtube video][yt] recently that linked to a [couple][c] 
of [papers][p] that suggested these extra layers don't always work that well.

I've recently been "building my own skills" a lot more.  It takes a bit longer
to go from "zero to one".  In the skills I've tried to build it's taken a few
iterations of adjusting it and testing to get what I'm looking for.  But at the
same time it's helped me understand what's going on beyond "install this and
now claude behaves differently".

---

There's an analogy I see with how we understand programming.  Something I've
long advised engineers to do is to find a rainy Sunday and try to write your
own hashmap.  Either approach it from a position of total naivety - just start
trying to implement something based on what you currently understand, and 
discover the nuances as you go (the better approach IMO); or do some reading
before you start.  Whatever you make or get stuck making, you'd never want to
use that in production.  But going through that exercise of exploring how a
fundamental data structure that you use every day might work, is very
enlightening.

I think the same is true for these layers of AI tooling.

What's different, in the case of a hashmap, is that the standard library 
(or otherwise) hashmap that you _would_ use in production, is most likely
_extremely_ well tested, optimised, and security hardened.  I don't think any
of this shit that we can install off the internet to plug into claude is by
any means "well tested, optimised and security hardened".

So why not build it all yourself?  You'll understand more about how things
work, you'll get something that is built for your use case, and with a bit of
testing and iteration you'll get something that is near enough as (not) robust
and (not) optimised as any of the things you can install off the internet.

---

I think also we're in a unique time where this is a good thing to do.

I see two possible futures.

One possibility is the agents and models improve, and incorporate the things
that these skills, hooks, frameworks, are attempting to achieve, as default
behaviour.  They can work on increasingly complex problems, without doing
dumb shit.  They better manage their context windows, deploying the right
strategy for doing so depending on what you're trying to achieve.  Out of the
box they can infer what workflow they need to achieve what you want to 
achieve, and all of these layers we're adding over the top become unnecessary.
The incentive is there for this.  If one company's agent works pretty well
out of the box, no config, no plugins, whereas another needs a specific
menagerie of things installed off the internet plugged into it to get it to
complete tasks successfully, I know which one I'll pick.

In which case we can delete all these layers we're adding in.

Or, this doesn't happen, and agents will continue to need lots of orchestration,
configuring, context management, and general corralling to deliver good results.
Then there would be the incentive for companies or someone to put the time and
effort into really testing and hardening and optimising these layers and
delivering them in a way you could install and get reliably good results out of
the box - good enough that you'd be willing to pay for it.  It seems that
attempts at this are already happening.  I wouldn't pay for it yet though as the
foundations of the models' and agents' behaviours that these build on appear to
be changing too fast.

In which case we can delete all these layers we're adding in.

So while we're in this state of: extra layers being needed, and what you can
build yourself with a small amount of effort is just about as good as what
you can install off the internet - why not have fun with trying to build this
stuff yourself?


[cct]: https://www.aitmpl.com/skills
[cek]: https://cek.neolab.finance/
[yt]: https://www.youtube.com/watch?v=GcNu6wrLTJc
[c]: https://arxiv.org/pdf/2602.11988
[p]: https://arxiv.org/pdf/2602.12670
[mlp]: https://tarquin-the-brave.github.io/blog/posts/2026-02-15-learning-from-claude-history/
