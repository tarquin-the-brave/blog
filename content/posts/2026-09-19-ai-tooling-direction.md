+++
title = "I'm Happy with how AI Tooling has Developed"
date = 2026-09-19T15:56:26+01:00
images = []
tags = []
categories = []
draft = false
+++

As an engineer it seems my journey with adopting AI tooling in my work has
followed a path familiar to many.  Last year it started with using Claude
and ChatGPT through the web, as an adviser, and has developed this year through: using
agents through the terminal, installing and building my own skills and harness,
trying other agent orchestrators,
hooking in MCPs and other hacky ways of connecting it up to sources of information,
ripping out harness layers as models improve, setting up OpenClaw to get a real
"work from phone" experience, finding ways to automate tasks beyond just coding,
orchestrating agents to be triggered automatically without me needing to kick
them off, and settling on a pretty minimal "out of the box" experience with
Codex/ChatGPT desktop app.

It's with the last of those where I finally feel happy with where it's got to.
Where I don't need to pour time and effort into a fiddly setup that I need to
maintain.  In [a post back in February][feb] I expressed my wish that the tooling
would go this way.  That you could take something "out of the box" and get good
results from it.  I'm glad to say that the time of "build the layers yourself"
has mostly passed, and we can all focus on the things we're actually trying to
build with the tooling, rather than tuning the tooling.

[feb]: https://tarquin-the-brave.github.io/blog/posts/2026-02-27-build-your-own-ai-tooling/

Throughout this time I've had many ups and downs.  Overconfidence, get burned by
reality, periods of scepticism about the whole thing.  But zooming out it's clear
to see the breadth of tasks and the confidence you can have in it have improved
drastically over this year.

# My Journey of Adopting LLMs

## 2024

I wouldn't really say my LLM adoption started in 2024.  Then it was limited to a
single task: "summarise this document".  But strictly speaking that was me adopting
LLMs, so I've given it a mention here for completeness.

## 2025

For most of last year, 2025, I was fairly settled with my way of leveraging LLMs
in my work.  I'd predominantly use Claude through the browser, and talk to it to
run through ideas, remind myself how to do things, explore how something could be
implemented, but I wrote all code myself.  It didn't have access to the codebases
I was working on or any wider context.  So I'd talk to it about things in the
abstract and translate that into code and what it means in my context myself.

There are certainly benefits of that level of interaction.  Because I was talking to
it in the abstract and translating that into how that manifested in my work myself,
I never felt like my understanding was running behind what I was doing.  Whereas
this year "am I understanding this enough", "how much do I hand over" have been
constant anxieties.  Although, I did on occasion catch myself asking it to remind
me how to do something like a case statement in bash, and then thinking "I shouldn't
need to be asking it how to do basic syntax...".

I do remember a moment when the capabilities of Claude through the web improved
dramatically.  When it got the ability to search the web itself.  Before then,
the information it had was limited to when its training ended and it would often
output quite outdated uses of libraries that have had many new major versions since
then.  Which also I think made it very easy for it to hallucinate.  For most of the
time if I ever pasted Rust code that it outputted it would not compile.  I learned
to use its examples as indicative of what to do.

One thing I didn't do back then, which I wish I'd tried more at the time, is the
"embedded in your editor" experience.  Like VS Code with Copilot or Cursor, where
you could write your function signatures with a comment on what the function should
do and leave the LLM to fill out the function's code while you defined the next one.
I think I would have enjoyed that experience, still designing the code and business logic at a low level
but cutting out the slower bit of typing it out and handling verbose boilerplate.
I don't have a particularly good reason why I didn't do that, other than "because Vim".
I never got round to configuring and hooking up Neovim to achieve that and couldn't
see myself using a GUI-based editor.

There were engineers around mid-2025 that were full-on "vibe-coding" with agents.
From what I saw that didn't appear to go so well.  I saw some engineers new to a
team that threw agents at their work without understanding what they were working
on, outputting lots of impressive-looking code that didn't actually work or solve
any problems.  They didn't last long.  I think it's still true today that the agents
need expertise driving them, and when approaching something new to you it's worth
learning what you can about it before getting agents to have their way with it.
But the difference now is much less stark.

## Early 2026

January 2026 rolled around, and the word was spreading that models had taken a big
leap forward in what they could do.  Like many, it was time for me to try out using
coding agents and install Claude Code.  I liked that it was in the terminal.  I think it was a smart move by them to make
the experience terminal-based, lending credibility to it amongst engineers like me
that historically avoided GUIs where I could.

I recall the experience back then.  If you took Claude Code "out of the box", and
just started trying to get things done with it, what it could do was not that
impressive.  It would get lost halfway through its context window, go off on tangents,
and often need as much work to rescue it as if you'd just written the code yourself.

This is where all the setup you had to put around it, the harness, really mattered.
There was already an ecosystem of skills and frameworks, like "GSD", that you could
install in an attempt to keep it on track.  Looking back it's hard to say how much
of a difference these really made.  And it was hard to be scientific about it.  With
such a wide open space of different ways of setting up the layers around the agent,
when it did stupid things: was it your prompts?  Was it how you set up the harness?
Was it how different things you've installed interacted in unexpected ways?  Were the
models just not that clever?  An economy of fear peddled by charlatans online didn't help either.

Given that installing a skill or framework was just a case of pulling in a bunch of
markdown files, and given that how these were distributed (often `npm install`) who
knows what else you were installing on your system: I started getting into a habit
of [making my own "skills"][feb] and uninstalling these pre-packaged ones.  While I
wasn't exactly running controlled and repeatable evals, building up my own agent
harness bit by bit and knowing what's in there started to give me some heuristic
understanding of how it was steering the agent.  When the choice back then was either
to: use someone else's markdown files or write your own (using an agent to help you),
I preferred the latter and it seemed to get pretty similar results.  Although I still
believe to this day that the results you get out of agents are less about the specifics
of your agent harness and more to do with your expertise in the domain the agent is
working in.  If you can easily validate what it's doing and quickly detect when it's
gone off course, you're going to get to a better place than someone that doesn't
know if what it's producing is right.

Back then "looping" was a thing being talked about a lot.  I don't know the extent
to which I really nailed the use of agents in this way at the time.  I had some
cases where I gave it a well-defined success criterion and told it to carry on until
it achieved it.  Later I developed that with [a multi-agent setup with an "Adversarial Team"][team].
But I didn't really adopt that approach explicitly for a wide variety of tasks, only
isolated cases.  It does seem that the idea of it has somewhat built its way into
the way the desktop apps of now manage the agent for you.

[team]: https://tarquin-the-brave.github.io/blog/posts/2026-02-27-adversarial-agent-teams/

One thing I learned to do, and still stick by to this day, is for each new model version
or change of model: delete all the harness and start from scratch.  I'd read things online
and had advice from people that a harness for one model won't work as well for the next,
as the idiosyncrasies of that new model's behaviour were different to the last.  But even
without that, however true or not it might have been, even without changing the underlying
model we all add things readily to our harnesses and rarely take things away.  Without
pruning things context windows would get bloated and confused.  So much of keeping the
agent on the straight and narrow back then was about managing its context window.

Reflecting on this, it speaks to a weakness in how we engineers would try to share the
steering of agents.  Committing an `AGENTS.md`, or `CLAUDE.md` into a codebase and loading
it up with everything any agent should know about the code and the domain it serves, sounds
like a good idea.  But when these don't get pruned and maintained well, everyone's agents'
context windows get bloated with crap that's piled up over time, and written to steer
the last generation of agents through their foibles.  It reminds me that these need to get
purged regularly as well.

Having been through this initial phase of adopting agents, I didn't feel certain that
I was dramatically more productive end-to-end.  Sure I could write code faster.  But then
there was this extra step of validating and rationalising the code that was just written,
which before you did as you wrote the code.  There were certain tasks where it definitely
performed better than I would have.  I recall a couple of cases where I set an agent on
a refactoring task of the shape of "here's a bunch of different implementations of fundamentally
the same thing.  Find the common pattern and generalise it to reduce the amount of bespoke
code for each use case".  It did a lot better at those than I think I would have been able
to in any reasonable amount of time.  I suppose something that is fundamentally doing pattern
matching is going to be good at finding common patterns.  I also had short bursts where I
felt I had supercharged productivity: jumping between 6 different tmux windows with an
agent running in each doing totally different tasks (which reminds me, git worktrees were
a must to enable this sort of work).  Although ultimately I found it quite draining to
continuously context switch between tasks to guide the agents and I couldn't do it
sustainably for more than a few days.

But I felt I could sense the potential in it.  I could see that the limiting factor was
my focus and attention to guide the agent through the next step of its work, and my
capacity to switch context while agents were working.  Therefore I saw the most potential
in agents that were given a specific task, with well-defined guard rails, that could be
triggered to start work without a human triggering them.  Agents triggered automatically
on events, rather than by human prompting.

> Automatically triggered agents that perform specific tasks were where I saw the biggest
win for overall productivity.

At this stage it was around April and at the
time Cursor already had a way of triggering agents automatically, and Claude's "Managed
Agents" had just come out.  But we tried those and they appeared to still have some
issues.  There's always the factor of "were you holding it right?", but the ease at which
people can use a tool right is part of the quality of a tool.

So I set about trying to set up the orchestration of automatically triggered agents myself,
for a specific use case.  The use case I chose was to create an early warning system for
when programs on Solana that our components interact with had changes.  Sometimes these
changes were breaking to our integrations.  Sometimes we'd get ample warning from the
maintainers of the programs.  Sometimes we wouldn't.  So I wanted an early warning system,
that could detect changes on either devnet or mainnet, investigate if it affects our
integrations and ideally send a PR to us with changes if we need to make them.

First I needed a service to listen for changes to the programs we care about and emit
an event.  This would have been needed anyway given the speciality of the use case, even
with AI providers having an easy way to trigger agent runs in the cloud on conditions.
It was a pretty simple service and Claude successfully one-shotted it.  Then I needed to
set up a Cloud Run job that would run an agent to do the analysis of whether our integrations
were affected and submit a PR if we needed changes.  The agent itself wasn't too hard:
some pre-defined prompts sent to OpenRouter.  But the whole setup of the Cloud Run job,
getting the right permissions, service accounts, etc, etc, was a real faff.  That
certainly made me long for a UI where one could define "if X happens, run an agent with
this prompt".  It worked OK in the end.  Using the models at the time and only having
a one-shot prompt has meant that sometimes the analysis is a bit off.  There are times
where it's concluded changes are needed then not sent a PR.  It was useful for a time.
Eventually after not maintaining it for a bit it stopped being as useful.  But it
served its purpose as an exploration into what automatically triggered agents could
potentially achieve.

## Mid 2026

For that first period of 2026, most of my usage of agents was via Claude Code in the
terminal, and I'd open each session within the git worktree of a single repo at a
time, doing the code management for the agent most of the time.  Early attempts at
letting it rip across multiple repos, and letting it cut its own branches and worktrees
went a bit too far out of my comfort zone with it.  "Why did you make these changes on `main`?"
came up a few times.  "Sorry I didn't follow my prompt...".  But time had passed, models
had got better, and I was interested to see what an experience abstracted further than
a particular worktree of a particular repo would look like.

Enter OpenClaw.  I'll admit, when I first saw OpenClaw gain popularity it seemed like
a terrible idea.  Seeing stories of people giving it unfettered access to their lives
and automating things that I'm not sure why they needed automating.  But with some
nudging from a colleague, showing me how they'd isolated it and used it for strictly
work I gave it a go.

What would elevate my agent experience beyond what I had with Claude Code was to have
it plugged into all our engineering tooling: Linear, Slack, Grafana, GCP, PostHog, GitHub,
etc, etc; and have a more convenient interface for me to use, that was more durable
and portable than keeping a tmux pane open with a Claude Code session in it.  I was
very cautious and ensured that it only had read access to any of our infrastructure
actually running services.  I set up a read-only Service Account for it in GCP rather
than letting it use my credentials.  There were some options on how I could interface
with it, but Discord was the clear winner.  Create a new channel for each session you
want to have with an agent, making it easy to switch between concurrent work, and providing
a history of conversations.  It was a faff to set everything up, but what resulted
was an interface to a lot of my engineering work that was common between my computer
and my phone.  For the first time I was able to get almost as much work done from
my phone as I could from my computer (minus things like actually deploying changes).
I called it my "WuFF system" - "Wurk From Fone".

It was still a bit rough around the edges.  At times agents would just crap out and
not respond in Discord.  I tried a few "cron jobs" with OpenClaw, but found it pretty
hit and miss if they would actually run at the time they were supposed to.
But for a few months I did a large proportion of my engineering
work via Discord talking to my OpenClaw.  I say "talking": I did also set it up so
I could send it voice notes rather than typing, but I didn't really use it that
regularly.  I can type fast enough.

It was around this time I moved from using Anthropic models to OpenAI models.  I can't
really express a measurable reason why.  I was switching between them a bit and
found that the OpenAI models were much better at short, to-the-point communications,
adhering to my "caveman mode" system prompt, and generally in code seemed to write code
that was less verbose and less over-engineered.

Earlier in the year, I'd played with "automatically triggered agents".  At this
point I (or we, my team) hadn't really pushed much further into that area.  But there
was one case where we'd been using them for a time: code review agents - CodeRabbit, Codex Connector,
Copilot, and Claude.  It makes sense why this was a use case for automatically triggered
agents that was made easily available.  Everyone wants code review.  For a time
we'd had CodeRabbit turned on for our repos.  But we hadn't given it much care and
attention and to be honest it was more annoying than useful.  But as models got better,
and these tools allowed more configurability to guide the review agents in your codebases,
we started making more deliberate use of them.  I think it's worked pretty well.
I say initially they were "kind of annoying" but that was from a perspective of
a human being bombarded with unuseful or unguided comments from a review agent.
But I realised that that was the wrong way to think about it.  Given that you're
developing the code through your agent that creates the PR, you can instruct your
agent to also await the comments from the automated review agents, decide for itself
whether the comments are "real" given all the extra context it has on the changes
you're making, and respond, fix, and resolve the agent comments before you pass the
PR to a human.  The obvious issues, the low-hanging fruit in the PR, are dealt with
between the agents, and the human review is reserved for the more impactful questions.


## Now

So that brings us to now, or recent times.  I don't use the OpenClaw "WuFF System"
anymore.  I've moved over to using the Codex desktop app (now called ChatGPT desktop app).
The model for how I interact with agents was set by that OpenClaw setup.  Just now
with the Codex app I get all of it out of the box.  What took me essentially a whole
day's worth of time to faff on with setting up the OpenClaw, plugging into all
our various tooling, took no more than 20 mins to get the same setup with the Codex
app.  I can get the same "Wurk From Fone" experience with connecting to a "remote"
from my phone.  And it just works smoothly.  Granted you have to be careful of their
upselling.  Each update tries to sneak you onto "Fast Mode" for a greatly increased
price of usage.  But to have something work out the box with a quick setup that surpasses the functionality
that just a few months ago took many hours to get, and far surpasses the capabilities
that one could achieve a few months prior to that with endless time tweaking and
revising one's setup - it's a big win.

Add Wispr Flow on top of that for whenever you can be bothered to type or it's quicker
to say something.

While the cron jobs "kinda worked but not really" on the OpenClaw setup, I've made
extensive use of "Scheduled Tasks", and largely use them for things that aren't
directly technical.  I've got [a regular task to update dependencies][deps], but
beyond that I've got scheduled tasks for all sorts of admin tasks: auditing our
release reporting, scanning for any bug reports that have been neglected, getting
a summary of what issues have been seen with the product that week, and getting
a high-level summary of what everyone on the team is working on.

[deps]: https://tarquin-the-brave.github.io/blog/posts/2026-09-19-dependency-updates-with-agents/

Another thing that's been introduced in my team recently has been agents that
are available to interact with in Slack.  Claude Tag, Linear Agent, PostHog Agent.
It's an interesting development that before recently, everyone's interaction with
agents was a private affair.  But now it's happening out in the open.  Having them
auto-respond to alerts, bug reports, and other related things is getting us closer
to a place where: when a human gets involved, the groundwork has been done, and the
human can do what they're in the best place to do - make informed decisions.  Like
I've said a few times above: it's in the automatically triggered agents that the
biggest productivity unlock lies.  If by the time you as an engineer are faced with
an issue, all the information you need is there, and you don't need to spend time
mudlarking around, your time and focus can be dedicated to decision making.

It's unclear to me these days how much context window management matters.  At the
start of the year it was everything.  With the current tooling the way it is, it
seems largely abstracted away from you.  But I don't know if that's just "hidden"
or actually elided.

---

So I'm pretty happy with where all this tooling has gone.  I still have lingering
concerns about "comprehension debt" - agents need expertise to drive them.  And I
do wonder a bit about the economics of incredibly heavy usage of agents that you
see on some YouTube videos.  But specialised event-triggered agents are where I
still see the greatest potential for increases in overall productivity.  "What can
agents reliably and repeatedly do without you being there to prompt them to do it?"
And the more "specialised" and constrained the task the less "frontier" the model
needs to be to do it reliably, which might make it actually economical to do it.
