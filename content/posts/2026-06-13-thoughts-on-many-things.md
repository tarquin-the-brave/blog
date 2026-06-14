+++
title = "My Thoughts on Many Things - 2026"
date = 2026-06-03T11:33:44+01:00
images = []
tags = []
categories = []
draft = true
+++

At the beginning of the year it would have been a decade since I started as a
graduate Software Engineer.  Had I been paying attention to that fact then
I could have written a post of "My Thoughts on things, 10 years into being a
Softare Engineer, having also been a Manager in parts of that time".  But instead I've only thought about doing this now.
Getting together my assorted opinions on things in Software Engineering that
I've accumulated over the years and I suppose most heavily influenced by
recent experience.  "My thoughts on things after nearly but not quite 10 and
1/2 years being a software engineer then manager then engineer again then 
manager again then engineer again".

Anyway, I'll get on with in.  In no particular order:

# Infrastructure / DevOps is Underappreciated

Without getting too lost in definition here I'm talking about how software is
build, deployed, and configured, how changes get delivered to the user, and how
automated, robust, safe, and fast that is.

Every team I've worked on there's been a feeling that we could do better in this
area, and we'd benefit from doing so, but it doesn't get prioritised.  I think
this is driven by a degree of myopia where it "works well enough for now", or
"works for me" and we don't always see how that compounds and products mature
and teams scale.  You might be one of the engineers who build most of a product
and administer the releases via a series of scripts that you run and watch the
output of, making manual interventions here and there, and see that as good enough,
but you don't see that as a bottleneck - if it was just you working on this it
would be fine, or see the danger in the concentration of understanding of how
the product actually runs in production and gets deployed.

It's easy to read books like "Accelerate" and largely agree with everything it
says, but then to make excuses why it, or parts of it, don't apply to your 
situation.  I highlight "Accelerate" because I think it's a great book and I've
actually read that a couple of times, but I've heard colleagues in the past
dismiss it as "it's just a bunch of statements of common sense".  And it is.
But then when you think about how your infra and devops lives up to it, asking
"why don't we have implemented everything it recommends? If it's such common sense"
the excuses start coming out.  In the past I've done this: "ah, but our company is
B2B and quarterly releases is what the customers want", "our stack is more 
complicated than 'your average web-dev' so...\[insert excuse\]".  What's been a
hard lesson for me to learn over time is that there isn't excuses.  Having great
infrastructure, and nailing those fundamentals that things like DORA metrics cover
is essential, and the sooner that can be achieved in a product's life cycle the
better.  I can understand for a new company racing to get the first version of
the product out there and seize the moment if they don't do the work to setup
their infrastructure well but as soon as it's out, that should be their top
priority.

I'm a big fan of "Walking Skeletons".  When you're building a new component,
getting a no-op version of it deployed into your stack, handling how that gets
deployed and configured with automation, how it interacts with other services
over deployment - especailly compatibility of interfaces, plugging it into
observability infrastructure, and knowing how every deploy is triggered,
validated, and rolled back when it needs to be, before filling out the
functionality of the component.  It's great, it's so much better than doing
any of this stuff as an afterthought.  It proves out architectural issues,
finding them sooner.  Once you have the Walking Skeleton, delivery becomes
much more predicatable as what's left is "write the logic for the service".
There's no risk of delivery pressure making you skip some of the infrastructure
setup or plugging in observability which will cause instant pain after you
first release it.
And you can deliver small increments and have that tested IRL.  If you're used
to building services by: writing the logic, testing in unit tests and running
locally, then thinking about how to get it deployed, try doing it this way
instead.  I think you'll be pleased with how it goes.

Feature flags are also great.  They allow you to decouple "deployment" from
"release" and makes rollback so easy.  You can deploy the service or the
changes to the existing service (e.g. new endpoint), validate that
everything is deployed as expected, monitor metrics, even re-run testing
in production either automated or by turning feature flag on for internal
users, all before your users are exposed to the feature - the release.
You do need to remember to do the tail task of removing feature flags once
something is fully released, else you end up with 2^n configurations for
your product, and dead code hanging around in the codebase.

I've talked about how infra and devops are underappreciated at the
organisational level, but what I see as more insidious is how it is
underappreciated in engineers' minds, their knowledge and skill sets.
I consider myself lucky.  The first place I worked at, a place where I
stayed for many years, we didn't have "infra people".  We didn't have
people who setup VMs and kubernetes, CI, CD, container builds, deploy
processes, for us.  We, as devs/engineers had to do it ourselves.  Sure
there were certain members in each team that were more into it than
others, but it was never "somebody else's problem".  I think it's so
important that every engineer has a good grasp of the operational concerns
of what they're developing.  How a service behaves is intimately linked
to the infrastructure it sits in, how it's deployed and rolled out, how
it interacts and communicates with other services.  If your service is
updated by it being stopped and started in situ in a long lived VM vs
if your service is running as a Pod in a Deployment in k8s or if the
service stays running and hot-reloads changes: you're going to develop
that service differently.  And as menitoned earlier if only a minority
of the engineers on a team understand how things are deployed and are
the ones left with the responsibility of administering deploys, you
have bottleneck.

That is to say that no matter if your team has "infra people", I think it's
still essential that every engineer understands how their services actually
run in production.  Operational concerns are a central part of development.
Ops is part of dev.  DevOps.

# Performance: "Don't Prematurely Optimise"

> "Don't prematurely optimise performance"

I agree.  But I think this statement gets wildly misrepresented and used to
justify the willful production of crap software.

I take that point to be: "don't delay delivery if the performance is already
good enough for how the software is going to be used"; "don't waste time on
improving the performance of components that aren't the bottleneck in your
system"; "don't sacrifice the maintentanability of code unless the speedup
is essential"; and more than anything "nothing is really fast or slow until
you actually measure it".

However, sadly, I've seen this point used time and time again as a smokescreen
for "I don't want to have to think about how a computer works, the efficiency
of my logic, and what the performance characteristics of my code might be",
throwing the statement in the face of anyone that tries early to raise the
issue of performance.

The statement around not overoptimising is about not incurring extra significant
cost when you don't need to.  But it's not to say "don't think about performance".
Where the cost of considering performance is negligable or nothing, like
when writing the code for the first time, you can save a lot of future pain
by making sensible choices.  Be aware of what latency you might expect each
extra network hop in a design to introduce.  Be aware of the algorithmic complexity,
the O factor, of your code, even if you don't optimise that.  When picking a
data structure, be aware of the performance characteristics of the operations
that you know you'll need to do on the data structure.

The reason I highlighted "nothing is really fast or slow until you actually measure
it" is because even things that seem like they should be quicker at a code
level might not be.  O factor isn't everything: it doesn't kick in until your n is 
sufficiently large; details of the real data you're processing can make the "worst case
O factor" not actually what happens IRL.
There's all sorts of things that are beyond my expertise about how modern hardware
makes certain operations quicker than others.  There's all sorts of caveats so unless
you've measured it you don't actually know - and even then you need to measure it the
right way.  But there's still basic heuristics that can guide you, like considering
algorithmic complexity, and thinking critically about "am I making the computer do
unnecessary work here?", which mean you don't leave a bunch of potential performance
problems lying around in the future.  And it's not wisdom to not consider these 
things.

In light of how I've seen this statement of avoiding premature optimisation get
misunderstood and misused, I find it better to think about software performance
in these terms:

> "Proceed with a weakly held affinity to the optimal"

Go into writing software with an intention of making it optimally performant
but eargerly let go of that ambition as you encounter hurdles, uncertainty, compromises.
Be aware of what likely would be the more performant way for the code to be
written, but abandon it when it becomes clear it'll have additional cost to it:
difficulty to implement, less maintainable or robust code.  I think that thinking
about code and software performance in these terms is helps to avoid being
ignorant, willfully or not, and causing such forseeable issues down the line.

I appreciate my perspective here is heavily influenced by Rust, the language I've
done the most work in in the last 8 years.  Generally Rust makes you explicitly
opt-in to things that might be not as performant.  You have to exlicitly `.clone()`
if you want to clone the data, explicitly `Arc` if you want to add indirection
to avoid the clone, `Box` for dynamic dispatch.  I've had colleagues in the past
say they didn't like this as "it makes you feel guilty for doing things that are
totally fine to do in most circumstances", but I quite like it.  The language gives
you a gentle nudge towards doing things in the least wasteful way, and if ever
you want to compromise you have to be explicit about it, which means you do so
knowingly.  I think the "guilt" only comes in if you feel a pressure to keep things
optimal in the face of hurdles, and that's why I like to think of this as a "weakly
held affinity".

# Cost

Where I talked about "performance" above I was really talking about "latency".
Operational cost is another aspect of software performance.  In a similar
vein to my thoughts on "performance" I have a similar view of engineering
costs in general.

I get the business case, that for a startup generally follows the pattern of:
growing revenue is far more urgent than optimising costs, don't be wasteful,
but if we can throw resource at a problem and have no delay or issues in
delivering the service that will grow our revenue then that is far preferable
to waiting around while we work out how to deliver things at a minimal cost.
And where the time spent to even talk about or consider costs makes it not worth
considering.  Engineers are given the beefiest laptops that money can buy,
infrastructure is deployed with wildly over provisioned resources, as it's
not worth it to spend time working out what the actual resource needs are
and not worth it to have problems if the resources are lacking.  Then later
down the line, when a larger scale is being served and the team has grown,
those costs start to become more important and you start to tighten up on
costs.

But cutting costs can be a harder than it would seem.  Painful rewrites,
re-architects can be required to get out of a costly situation.  Like with
performance I'd say approaching things from the beginning with a weakly
held desire to do things cheaply but being happy to go more expensive in
stead of getting bogged down trying to be cheap is a better way of thinking
about it.  There's also a ego issue I've seen around spend - "look how much
we're spending on this, we really mean business".

I think there's an extra technical angle on this that tips me towards wanting
to think about costs earlier.

> "Necessity is the mother of invention"

If all your engineers are developing on a top spec laptop, is it really a
surprise if users later complain that the application or site doesn't run
well on their laptop or browser on a mobile?  Especially for frontend
engineers, if they're given low powered laptops to work on they'll develop
a site that runs well on any laptop, to the benefit of users.  If backend
engineers have low powered laptops they're going to make improvements to
builds that will make them quicker in CI, reducing lead and cycle times,
ultimately fixes get out quicker, and they'll reach for developing and testing
the application in an environment far closer to the production environment
than their local machine, developing the tools and skills that will be
useful to debug issues in production environments.

That is to say that I think working with an inital assumption that resources are
limited, eargerly relaxing that if it causes a problem, will generate better
solutions and fewer technical and fiscal headaches down the line then developing
with a view that resources are unlimited.

# Optimism in Delivery Deadlines

Having been an engineer this whole time and a manager for intermittent periods,
I've seen both sides of engineers and management struggling together or against
each other to deliver software at a predictable time.

It's really hard.

I certainly don't have all the answers here on how to ensure timely and predictable
delivery of software.  If I did I wouldn't make it free to read on the internet.
I wouldn't trust anyone who claims to have this all worked out.

I think a key mistake that managers (or other managementy types) make is to not
be willing to shoulder any of the uncertainty around software delivery, and to
try to push that responsibility entirely down onto engineers.

Attempting to manage delivery and task management to a granular level can get you
to a point where numbers you associate with delivery look consistent and predictable,
but ultimately less is getting delivered as more and more time is taken up managing
the constructs you've made around the work, rather than the work itself.  And by
pushing the responsibility for delivery entirely down onto engineers, they then become
defensive, to create room for the uncertainties that come out of the reality of
developing software, and so much energy is wasted in an unproductive negotiation between
engineers and management on when they'll get something done by.  When the goal has
always been the same: to deliver as much user value as possible as fast as possible
over a sustained period.

Take "SCRUM" for example.  I've been on teams before where almost half an engineer's
calendar is taken up with meetings to discuss the work they're not doing because of
all the meetings: Sprint Planning, Technical Ideation, Backlog Refinement, Backlog Grooming,
Techincal Refinement, Standup, Standup, Standup, Sprint Demos, Sprint End, Sprint Retro,
all on top of weekly 1-1s with their manager where they end up talking about the work they're
doing again.  Do all of those meetings produce such incredible results in the time between
them that they make up for the time they took?  No.  It demotivates engineers as well.
If they're ambitious and want to get things done, they're punished when they don't complete
all the tickets they allocated into the Sprint.  The incentive is to commit to as little
work as possible.

Whether consciously or not, and I'm happy to assume not, I've found that engineering work
pretty much always fills the volume of its container - and has no issue spilling out of
that volume.  That is to say that if engineers are in a situation where the target
delivery dates they're working to are very comfortable, the work will still be delivered 
at a point right up to the deadline.  I think it's wasteful, and ultimately demotivating, for
engineers to be working to unambitious deadlines - which is what results from management
managing delivery to be predicatable and comfy for them, not having to shoulder any of the
uncertainty, and convenient for planning.

Convenience for planning is a good thing.  Especially as an org grows being able to effectively
plan ahead and know what capacity is needed where to achieve things is great to have.  To
some extent I think it makes sense to sacrafice some amount of delivery for that delivery
to be predictable.

So I think it's best for engineers to working towards ambitious deadlines.  It helps to
avoid unnecessary work and is ultimately more motivating to have a goal that one needs to
work hard to achieve - rather than being comfortable.  But I don't think it's good to
push overly ambitious deadlines down onto engineers.  It's better that the drive to deliver
is coming from within them.

Like I said I don't have all the answers here.  I don't have foolproof method of managing
delivery that always succeeds.  But there are some general principles that I try to follow
when handling delivery.

Ideally the drive for delivery is coming from the engineers.  They're motivated to deliver
value to the users and want to do that as soon as they can.

Coming up with ETAs and deadlines should be collaborative.  It's not productive for it to be
a fight between engineers and management / product.  We all have the same ultimate goal.

The general pattern I try to encourage is one where engineers can set _optimistic_ and _realistic_
ETAs, then managers, leadership, or whomever is trying to coordinate delivery across the org
gets an idea from the engineers on what a sensible amount of contingency is needed.  If one
project has a dependency on another, you don't want to plan to start that second project on the
day of the optimistic ETA of the first.

Engineers setting and working to _optimisitic_ and _reaslistic_ deadlines and management
taking responsibility for the uncertainty and adding contingency.  Not the engineers being
made wholly responsible.  If in doubt, take what the engineer says and double it.

# Discipline of Thought in Engineering

It's weird that we call ourselves "engineers".  It's not like it's an accredited profession,
and it's hard to get any two software engineers to agree on what good engineering even is.
It could just be that things change so quickly in the industry, whereas the laws of Physics
that govern "soil mechanics" haven't changed for Civic Engineers ever.  Unlike "real engineers"
software engineers aren't working on the same shared foundation of understanding that Engineers
would get from their Engineering degree or industry exams and accreditations.  Opinion and
feelings seem to be very present in technical discussions that us software engineers have.
I find conversations get a bit stuck as they lack rigour.

Aside: I think the introduction of instant message application has degrated the rigour and discipline
of engineering thought and communication.  Every thought, as a new message sent instantly.  Threads
that don't read in order because of the race condition between folks typing.  Imagine instead if
techincal discussions happened over email trails: each message being thought out and crafted
with complete thoughts, responses inline.  In fact I was a year into my first job when the company
I worked for introduced Slack as an instant messaging service.  Before then, and for some time
after out of habit, we did long form techincal communication via email.  It's somewhat impossible
to imagine now a workplace without instant messaging, and I wouldn't propose it, "monkey's out of
the bottle", "there's no going back now".  But it does make me think when I realise just how
recent an addition to the workplace instant messaging is.  People coped fine without it.
Especially for a remote team, it feels that instant messaging is essential, however I feel any detailed, 
long-form technical discussion in better had elsewhere, not least as instant messengers are terrible
for posterity - finding the conversation months later.  If anything starts to get a bit involved
in Slack I try now to move it over to comments in a ticketing system.

There's this short YouTube video "Three analytical traps in accident investigation" that I watched 
a while back as part of some "incident reponse training". I've found it very useful to help with
thinking in a disciplined way when dealing with potentially urgent issues.

https://www.youtube.com/watch?v=TqaFT-0cY7U

It's not actually about software, it's about aircraft incident investigation.  But then again it's 
about "complex systems with layers of automation". so it is totally about software.

Honestly no single video made more of a change to me as an engineer than watching this one (all 
7 mins 36 seconds of it).  I'd recommend: watch it, make notes, watch it again.  Then next time you're
investigating a bug or incident or even building a feature keep "the three logical traps" top of mind.

I've been thinking a lot about data integrity recently and that "mechanistic reasoning" trap is a
really easy one to fall into there.  A system is built to work correctly provided the data is all 
correct, perfect.  When it isn't all hell breaks loose.  Cause: components performed as they should, 
data was incorrect.  Fix what caused the data to be incorrect.  But in a sufficiently complex system
it becomes practically impossible for all data to be correct all of the time.  A robust system is one
that can detect incorrect data and heal it, proceed when data is imperfect, or alert if it can't.
When it comes to how components work it's shifting assumptions from "if everything else is working
correctly this should work" to "assuming there are going to be bugs elsewhere, and data is going
to be imperfect, what opportunities does this component have to: monitor, detect, and fix it".

# Use of LLMs

I'm somewhat hesitent to write anything on this topic given that we live in a state of being
one new model away from everything changing.  But I suppose that's the first 
