+++
title = "Dependency Updates with Agents"
date = 2026-09-19T13:12:34+01:00
images = []
tags = []
categories = []
draft = false
+++

A big benefit of using agents to assist with engineering work is not necessarily
the increased pace that code is written, it's only one small part of the software
delivery life cycle, and if all you're achieving is writing code faster, at best
you've moved the bottleneck.  Really the benefits come from how it can protect
your time and focus to stay on higher impact decisions.  Generating code helps
somewhat in that regard.  The time spent typing the code and making localised
implementation decisions, that can be somewhat arbitrary, is handled for you.
But applying agents to perform other time-consuming work outside of the code
implementation is where a lot of that benefit of protecting that time and focus
can come from.  There's a lot more to engineering than writing code.

Finding those cases of work that might have been historically hard to automate,
disparate fuzzy interfaces, time-consuming but not "high brain power" tasks, and
giving them to agents, is a big unlock.  Updating dependencies is an area where
I've been historically disappointed with automated solutions and have found that
it's a great singular task to give to an agent.

---

# Historical Experience

When you start a project naturally the dependencies are up to date.  And if you
proceed with development of the project always looking if you can update dependencies
every time you work on it, then there's not really a problem to solve here.  But
realistically that doesn't happen.  First you need to know that there are new versions
of your dependencies, which without any tooling would require people check.  Then
not every dependency is as simple as changing a number in a dependency file to update.
Often you have to change the code using that dependency to accommodate the update.

Updating dependencies can come with subtle risks, side effects not represented in the
interface to those dependencies.  One particularly nasty example I've seen from working
in Rust, is when a new version of a crate changes the "features" around on a crate.
"Features" on Rust crates are usually used to reduce the size of the crate and number of transitive dependencies
for anything depending on it when it doesn't need all of the functionality of the crate.  The crate
can by default expose a smaller set of functionality and have features that provide
more.  But crate features are simply compile-time flags, conditional compilation.
There are cases where the feature(s) you select when adding the crate as a dependency
change the runtime behaviour of the crate you're using: changing the underlying algorithms
they use, or in some cases changing how a structure is serialised for the wire.
I've had a case of the latter where updating a dependency and turning on a feature in
that crate, resulted in the wire serialisation of a message that a different service
than the one I was working on that was in the same cargo workspace having the serialisation
of the data it emitted change, which only happened later when that other service was
updated and released, breaking the deserialisation of that message in a downstream
service.

So you get into a situation that I'd think all engineers are familiar with, where
a significant number of your dependencies are out of date, and it's a fairly major
job to update them all.  That task to update them is important, but not necessarily
urgent, and gets put off.

What provides the urgency is if a security vulnerability is found in a dependency
you're using.  The more out of date dependencies are at that point, the harder
potentially is the update that you now urgently need to do.

Historically there has been plenty of tooling to assist with this.  Dependabot, Snyk,
etc.  But I've always found that in reality they haven't taken away much of the
manual work.  They do have a value in detecting the opportunity or need for a dependency
update.  That saves the first step of needing to check for these yourself.  They can
be set up to bump the dependency for you and send a PR.  But in many cases the manual
work and focus was still very much needed.  You get a PR that bumps a number in a dependency
file.  The CI on the PR is now failing as there was a breaking change in that dependency.
You're still left to do that work to change the code for the new dependency version.  And
you have to prioritise that.  You need to know how important that is.  The tools may have
included details in the PR of any CVEs that were raised against the dependencies, but
it doesn't show you if your code is actually affected by those CVEs, whether the code
path in the dependency called by your code is touching the vulnerability or not.  The
answer to that dramatically affects the importance of you choosing to focus on fixing
that broken dependency update.

Of course in an ideal world, you're up to date with all your dependencies all the time
and if an automated dependency update results in a broken build you can quickly deal
with that one case, a quick bit of manual work, and you're all up to date again.  But
even a single dependency update isn't always that simple.  A large dependency that is
heavily used in your codebase having a new major version, or getting deprecated in
favour of a new framework, or being split into smaller parts, can result in a major
job to update that one dependency.  So the reality is that we fall behind on dependencies.
A pragmatic choice could be to say that we'll only update the dependencies that we _need_
to.

To summarise: historically I've found tooling around dependency updates to be lacking
in two main areas:
- They don't do most of the manual work to update dependencies, you're left to tackle
  code changes to accommodate breaking changes in the dependencies.
- It's not made clear the _need_ for the update: _if_ you're actually affected by a CVE,
  what your code could stand to gain from the new features or interfaces of the dependency.
  You're left to do the manual work to find that out, which means you're already committing
  your time and focus before you know how important it is.

# How Agents can Help

My two main historic gripes with "dependency update tooling" were:
1. They just bump a dependency version in a file, not the work to accommodate that update,
2. They don't hand off to you a full picture of why the update is important.

These are tasks that you are left to do yourself.  These are very much tasks that I described
above:

> work that might have been historically hard to automate, disparate fuzzy interfaces, time-consuming but not "high brain power" tasks

A scheduled task with an agent.  In the prompt you can specify exactly how you want it
to approach the problem, given the constraints and context of your project.

This year I did exactly this.  My starting point was a collection of Rust repositories where
dependencies were very out of date.  So the most urgent priority was to ensure we made
updates for any CVE, and to narrow that down, CVEs that were actually reachable from our code.

My prompt instructed the agent to start from `cargo audit`.  Where CVEs were found to trace the
call path via `cargo tree -i` (inverted dependency tree up from the affected dependency), and
then to inspect the source code of the affected crate, the source code of the intermediate
dependencies between the affected crate and our code, and our source code to attempt to establish
if our code is exposed to the CVE.  If it couldn't conclude that the vulnerability was unreachable,
update the dependency.

This was the strictest minimal set of dependencies that we _needed_ to update, which I chose
to go for to reduce the surface area of updates in the first pass of this, focusing on the
most urgent fixes first.

Through the prompt I was able to specify how it should present the changes in a PR, grouping
"just dependency bumps" and very minor code changes into one PR, and separating dependency
bumps that require larger code refactors into their own separate PRs.  I required from the agent a detailed
breakdown in the PR description of: exactly what the CVE vulnerabilities were, an explanation
as to why they're a problem, and how they are reachable from our code.  I also instructed it to
await automated agent PR reviews and resolve all comments before sending me the PRs.

That's the beauty of it: that you can specify exactly how you want it to do things.  While I've used
it very specifically to go after "vulnerabilities that affect our code", the same approach could
be taken with a broader set of dependency updates.

While not every attempt of mine this year to get agents to do things has worked this well, I was
pretty happy with this one.  At the point where _I_ need to pay attention and engage my brain,
there's already a PR that provides a detailed trace of why the upgrades are needed, makes the
required code changes to accommodate the updates, and has been through a round of code review.
It's still not the case that I just merge it, I don't trust agents that strongly yet and I want
to see what they've done.  But the work that I have to do is greatly reduced and importantly
I don't have to do a bunch of work to discover something isn't even a problem that needs fixing
urgently.

As this application of agents has worked pretty well for me, I've not actually checked the
latest art of what these "dependency updating tools" offer right now.  I'd imagine it wouldn't
be hard for them to do exactly what I've done here and have their tooling intelligently update
dependencies according to your preferences.

# Conclusion - and a Warning

This case, a more intelligent approach to automated dependency updates, is one where applying
an agent to the problem has worked great.  The way tooling has historically lacked in this
area and the low brain power manual work that resulted was because of "fuzzy interfaces".
LLMs are great at dealing with fuzzy interfaces, such as language.

But consider another case where perhaps historically you might have had time-consuming manual
work to pull together disparate pieces of information with fuzzy interfaces.  Consider having
a system with lots of components, where debugging a problem might require looking through the
logs and metrics of multiple services.  Services that were built at different times by different
people, in different programming languages.  Historically that might have taken considerable
manual work to check through and cross-reference different reporting information from different
services, and a certain amount of tribal knowledge to boot.

Before LLMs this would have been a real friction that could greatly increase the time to resolve
a critical bug or an incident.  And historically, before LLMs, you might be motivated to fix
this by undergoing a big piece of work to commonise log formats and schema into a machine-readable
format, passing trace IDs through communications between each component, and setting up a way
for engineers to easily trace the flow of a request through various components of a system.
Essentially: do the work to turn the fuzzy interfaces into well-defined interfaces.

These days, you could just have an agent hooked up to your observability and ask it to go work
out what's happened, tracing the disparate logs and metrics from different services to piece
everything together.  It'll do it.  But it'll take time, could be inaccurate or misattribute
things, and will be expensive in token spend to perform.  It's still better to fix those
fuzzy interfaces to the extent that an agent isn't needed, or you hook the agent into the
well-defined interfaces for convenience.

That is to say:

> Agents are great at tackling fuzzy interfaces, and can save a lot of human
time in doing so.  But they should not be an excuse to not fix those interfaces if they can be
made to be better defined.
