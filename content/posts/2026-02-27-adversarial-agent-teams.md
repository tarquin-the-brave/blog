+++
title = "Adversarial Agent Teams"
date = 2026-02-27T14:07:37Z
images = []
tags = []
categories = []
draft = false
+++

I had the situation recently when I was working directly in Claude Code.  I'd
got some feedback points for minor tweaks to make to a frontend implementation
I was working on, so thinking these were each pretty small and simple things
to resolve, I pasted the list of them into Claude Code and asked it to deal with
it.

This didn't go very well... It fixed the first point.  Then ignored the rest
and said it was done.  I had to then hand-hold it through each point.  It
kept claiming things were fixed when they weren't.  It took a lot of iterations
to apply the changes - which were all cosmetic changes.

But, as every cloud has a silver lining, this did provide a decent test case
for a different approach to orchestrating and corralling agents.  I could
wind back to the commit before these fixes were made, and enter the same list
of feedback points as requirements.

---

Since adopting Claude Code this year, most of my "building stuff with it",
and certainly all of what has gone into production, has been me working with
it interactively.

It's been great for pushing into areas that I was unfamiliar with.  Researching
technical options, validating ideas, and writing code has been a lot quicker.
Writing code in areas that I wasn't so familiar with has been _way_ quicker.
When working with a language and a domain I'm less familiar with, I still feel
like I'm coding, but working at a higher level of abstraction.  Rather than
scrutinising the code it's proposing to write, I'm scrutinising the logic
and where the data is going - as I don't necessarily know exactly what code
primitives are right in that language.

While there's those certain areas that feel loads faster, I've been wondering
really how much is using coding agents accelerating me.  There's other things
that take longer, or don't exist when coding manually.  There's a considerable
time spent discussing things with the agent to an extent that you're satisfied
the code and solution is sound.  When there's an issue with some of the work that
you've done with Claude in an area you're not so familiar with, it takes longer
to dig into that.

I think that if the extent of my usage of AI agents is "interactive" then 
there's a limit to how much it's going to accelerate me - I don't see whole
number multiples of productivity coming from it.  To work with an agent
interactively: what is that really giving me?  I can work with things that
I don't understand and type code fast.  Why don't I put the effort in to learn
about the things I don't know about and learn to type faster - a few touch
typing tutorials and getting better with vim keybindings and macros should
do the trick.  Then I don't get the productivity hit of debugging and refining
code that I didn't write or fully understand.  I'm not being entirely serious
here.  In reality getting fluid with any programming language takes ages, and
so agent-assisted coding in languages you're not fluent in is going to be
quicker despite the time to interrogate the agent on why the code will work.
And you can always [learn from the work that you do with the agent as you go][l].

[l]: https://tarquin-the-brave.github.io/blog/posts/2026-02-15-learning-from-claude-history/

My point is that without having agents execute non-trivial work autonomously,
where we can set and forget it, we won't see a huge multiple in productivity.
My vision of what my working day would look like when I've got where I want to
with this AI tooling is one of: Agents working autonomously on a collection
of easier tasks, or tasks that take a long time but aren't hard, or tasks which
are not high leverage critical tasks, the boring bits, while I focus on the
hardest problem of the day interactively with an agent, and only very occasionally
checking up or reviewing what the autonomously working agents are doing.

Autonomous agents is where the big productivity unlock is in my view.

---

I'd been wanting to give the new "[Agent Teams][at]" a go in Claude Code.  What
stuck out to me as a potential benefit of this is to have agents scrutinise each
other's work without them each biasing their judgement.  Something I'd found
with sub-agents is the main agent "leading them" to a decision.  I'd tried a
skill I found somewhere to "hand over to Codex" for a given problem.  When I 
tested this out on something I was going round in circles with Claude on, it
didn't work very well.  And when I looked into the "handover doc" it created
for Codex it fully misled Codex on what the problem was.  After I prompted
Claude to try again and specifically not to mislead Codex in that way, it worked,
Codex solved the problem and explained it in a way that made sense to me.

Setting the necessary "experimental" feature flag in Claude Code config I
started poking around with Agent Teams.  What seemed quite immediately obvious
was that to use these directly in Claude Code, you need to put a fair
amount of description into your prompt about what the team members are and
what their purpose is, and how and when they should interact.  In the first
couple of examples I tried, most of my prompt was in instructing the team
orchestration, and only a small part was actually what I wanted to do.

[at]: https://code.claude.com/docs/en/agent-teams

And what do we do when we find ourselves prompting the same thing over and
over again... we reach for a skill.

So I went looking for a skill that would orchestrate a team of agents for me.
Specifically I was interested in the team of agents being "adversarial" where
"teammates" (if that's what we're calling them) will mistrust and scrutinise
each other's work, and be as independent as possible.

I had a look around on the internet for something that would do this.  At the
time I couldn't find anything that was specifically aiming to orchestrate
"adversarial teams", but that was a week ago, so there's probably hundreds of
them now.  But, as I made the point in [my last post][mlp], there's a lot of
benefit to rolling your own skills and frameworks.  So I gave it a go, working
with Claude Code to build a skill to orchestrate an "adversarial team".

[mlp]: https://tarquin-the-brave.github.io/blog/posts/2026-02-27-build-your-own-ai-tooling/

I'll admit, I didn't completely start from scratch.  Going against the point
from [my last post][mlp], I built on top of skills taken from the [Context
Engineering Kit][cek], for each team member agent to leverage for its role.
But I did define the team structure and roles myself.

[cek]: https://cek.neolab.finance/

I wanted an interface where I only provide the requirements, then the team:
- work out how fulfilling those requirements breaks down into tasks,
- plans for implementation of each task,
- implements the tasks,
without my involvement.

Each of these stages has an "adversarial review" where a new agent is only
given the requirements and the output of another agent and asked to determine
if what's been done is right.

For the tasks breakdown, parallel agents are asked to each break down all the
requirements into tasks, then another agent acts as an arbitrator to resolve
conflicting breakdowns or asking them to try again if there's too much conflict.

The implementation planning has reviewers for each task's proposed implementation,
and a reviewer to assess consistency between the individual task plans.

For each point of review, if the reviewing agent isn't satisfied, the agent that
produced the work tries again and resubmits for review.

---

So did this work?

Once I had this built and did some testing and tweaking on what it reports and how
the agents interact, I turned it towards the issue that triggered this whole
exploration: a list of feedback points for some frontend work.

I rolled back to the same git commit I'd been at before, pasted in the list of
feedback points, much the same way I did when using Claude directly, and left
it to it to see what it could do in one shot.

It did a lot better.  7 out of 8 of the feedback points it fixed up as expected.
The one it couldn't do, no matter how hard I tried tweaking this and that in the
skill, was putting text that was spread across two lines onto just one line.
I don't know why, but it just couldn't manage this.

I'm going to keep using this, and trying it for different types of tasks.  This
will likely end up with me changing how this skill works.  But I'll share the
current version below.

---

Where's all this going?

Like I mentioned in [my last post][mlp], my hope for this technology is that
agents get good enough at doing the right thing and managing their own context
in a way that fits your given use case sufficiently that all these layers of
skills and hooks and orchestration that we're adding is rendered redundant.

But while I'm building stuff to play around with I want to try to push the
interface to me, the engineer, to as high a level as possible.  I think about
when a lot of engineers are happiest - sitting round a whiteboard discussing
things.  What if my technical work could look like that most of the time?
Could I be hand-drawing diagrams and talking and have agents implement
everything for me?

I'm going to try and find out.  Next on my backlog of things to play with is
to see if I can take this "adversarial team", but change my interface into it
to one of where I record myself talking while drawing on a whiteboard and
give it that.  I can see this one being a total car crash... we'll find out.
Stay tuned...

---

To use "Agent Teams" in Claude Code, you need to enable it in `~/.claude/settings.json`:

```json
{
  "env": {
    "CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS": "1"
  }
}
```

The agents in this skill use skills from the `sadd`, `reflexion`, and `kaizen`
plugins from the [Context Engineering Kit][cek].  To use it you'll need them
installed.

"Adversarial Team" skill.  Here's my current version of the adversarial team
`SKILL.md`.  Pop it in `~/.claude/skills/adversarial-team/`.

By all means try it, play with it, adapt it.  But also, I'd encourage
that you [build your own][mlp].

````markdown
---
name: adversarial-team
description: >
  Spin up an adversarial agent team from a list of requirements.
  Two planners independently analyse the codebase, an arbitrator
  resolves disagreements, per-task designers produce verified
  implementation designs, implementers execute with self-checking,
  and a reviewer adversarially verifies every result.
---

# Adversarial Agent Team

You are an orchestrator.  You create an agent team where the task
breakdown itself is adversarially scrutinised, implementers
self-check with `/do-and-judge`, and a dedicated reviewer tears
apart every result with `/critique`.

There are no shortcuts — even single-task projects get full
adversarial treatment.

## Input Format

The user provides a list of requirements (numbered or bulleted).
The URL for visual verification is requested later if frontend
work is detected.

Example:

```
/adversarial-team

1. Add auth middleware
2. Create user settings page
3. Wire up protected routes
```

## Procedure

### Phase 0: Pre-flight Check

Before doing any work, verify that all required skills are
available.  The following skills must be installed:

- `/do-and-judge` (sadd plugin — implementer self-check)
- `/critique` (reflexion plugin — reviewer multi-judge)
- `/cause-and-effect` (kaizen plugin — arbitrator fishbone)
- `/analyse` (kaizen plugin — arbitrator waste/flow)
- `/why` (kaizen plugin — arbitrator root cause)

Check: scan the available skills listed in the system
prompt.  For each required skill, confirm it appears in the
skills list.

If any are missing, stop immediately and print:

```
Pre-flight check FAILED.

Missing required skill(s):
- {skill name} ({plugin} plugin)
...

Install the missing plugin(s) and retry.
```

Do not proceed to Phase 1.

### Phase 1: Parse Input

1. Extract the numbered/bulleted requirements into a list (store
   as `{REQUIREMENTS}`).
2. Capture the current working directory as `{PROJECT}`.
3. If the user's message includes images (screenshots, mockups,
   designs), save each image to `{PROJECT}/.adversarial-review/`
   using the `Write` tool (or `Bash` with `cp`).  Store the
   file paths as `{REFERENCE_IMAGES}` (list).  If no images,
   set `{REFERENCE_IMAGES}` to an empty list.

Print status:
```
Parsed {N} requirements.  Project: {PROJECT}
{If REFERENCE_IMAGES: "Saved {count} reference image(s) to
{PROJECT}/.adversarial-review/"}
```

### Phase 2: Detect Frontend Work

Scan `{REQUIREMENTS}` for frontend signals: CSS, layout, font,
animation, responsive, visual, colour, color, spacing, hover,
border, margin, padding, width, height, opacity, transition,
gradient, shadow, z-index, viewport, breakpoint, media query,
component styling, theme.

Also check `{PROJECT}` for frontend indicators:
- `package.json` containing react, vue, svelte, next, nuxt,
  angular, or similar
- Presence of `.css`, `.scss`, `.sass`, `.less`, or `.styled.ts`
  files
- Tailwind config, PostCSS config, etc.

If frontend work is detected, use `AskUserQuestion` to ask the
user for a verification URL:

```
Frontend changes detected.  Do you have a local dev server URL
for visual verification?  (e.g. http://localhost:3000)
```

Options:
- "Yes, here's the URL" (user provides it)
- "No, skip visual verification"

Store the result as `{URL}` (empty string if user declines).

Print status:
```
Frontend detection: {detected|not detected}
{If detected and URL provided: "Visual verification URL: {URL}"}
{If detected and declined: "Visual verification: skipped"}
```

### Phase 3: Adversarial Task Breakdown

This is the key phase.  Two independent agents analyse the
requirements and propose task breakdowns, then a third agent
arbitrates.  If the arbitrator's confidence is low, the planners
revise and the arbitrator re-evaluates — up to 3 total rounds.

**Step 1 — Spawn two Planner agents in parallel**

Print status:
```
Spawning 2 independent planners to analyse the codebase...
```

Use the `Task` tool twice in a single message, both with
`subagent_type: "general-purpose"`.

While waiting for planners to return, poll `TaskList` every
~60 seconds and print a brief status line so the user sees
progress (e.g. "Waiting for planners... 1m elapsed").

Each planner's prompt:

```
You are a task-breakdown planner.  Your job is to analyse a set
of requirements against an actual codebase and propose how to
split the work across parallel implementers.

Project path: {PROJECT}
Requirements:
{REQUIREMENTS}

Instructions:
1. Read the codebase at {PROJECT} — specifically the files likely
   affected by these requirements.  Use Glob and Grep to find
   relevant files, then Read them.
2. Assess which requirements are independent and which are
   coupled.  Justify every coupling or independence decision by
   referencing actual code (file paths, shared state, component
   dependencies).
3. Determine how many implementers are needed (ceil(N/2) is a
   starting point, adjust based on coupling).
4. Propose task groupings and execution order.

Output this exact structure:

TASKS:
- Task 1: [description] → implementer-1
  Files: [list of affected files]
  Independent: yes/no (reason citing code)
- Task 2: [description] → implementer-2
  Files: [list of affected files]
  Independent: yes/no (reason citing code)
...

PARALLEL GROUPS: [[task numbers that can run simultaneously]]
SEQUENTIAL CHAINS: [task A → task B where B depends on A]
IMPLEMENTER COUNT: [number]
```

When both planners return, print:
```
Both planners returned.  Spawning arbitrator...
```

**Step 2 — Spawn Arbitrator agent**

After both planners return, spawn a single agent with
`subagent_type: "general-purpose"`.

The arbitrator's prompt:

```
You are an arbitrator.  Two independent planners have analysed
the same requirements against the same codebase and produced task
breakdowns.  Your job is to produce the definitive breakdown
using structured analysis — not ad-hoc reasoning.

Project path: {PROJECT}

Planner A's breakdown:
{PLANNER_A_OUTPUT}

Planner B's breakdown:
{PLANNER_B_OUTPUT}

Instructions:

1. IDENTIFY DISAGREEMENTS
   Compare the two breakdowns.  Note every difference: groupings,
   coupling assessments, file lists, implementer counts.

2. FISHBONE ANALYSIS
   Run /cause-and-effect with the problem statement: "The task
   breakdown for these requirements may have structural flaws."
   Provide both planners' breakdowns as context.  The six
   categories to examine:
   - People: ownership clarity, skill matching
   - Process: decomposition methodology
   - Technology: hidden coupling in the codebase
   - Methods: slicing strategy appropriateness
   - Environment: external/team dependencies
   - Materials: shared APIs, models, specs

3. WASTE AND FLOW ANALYSIS
   Run /analyse on the merged task list, treating tasks as
   workflow stages.  Look for:
   - Blocking dependencies that create waiting
   - Speculative tasks (overproduction)
   - Vague acceptance criteria (defect risk)
   - Handoff bottlenecks between implementers

4. ROOT CAUSE DRILLING (if needed)
   If steps 2-3 surface suspicious dependencies or coupling
   decisions, run /why on each one to drill to the root cause.
   Only do this for specific, concrete suspicions — not
   speculatively.

5. CODE VERIFICATION
   For each disagreement and each issue surfaced by the kaizen
   analyses, read the contested code sections yourself.  Do not
   trust either planner's claims without verification.

6. PRODUCE FINAL BREAKDOWN
   Synthesise all findings into the definitive breakdown:

FINAL BREAKDOWN:

DISAGREEMENTS:
- [description]
  Planner A said: [summary]
  Planner B said: [summary]
  My judgement: [decision with code-based reason]

KAIZEN FINDINGS:
- Fishbone: [key findings from /cause-and-effect]
- Flow/Waste: [key findings from /analyse]
- Root causes: [from /why, if run]

TASKS:
- Task 1: [description] → implementer-N
  Files: [list]
- Task 2: ...

PARALLEL GROUPS: [[task numbers]]
SEQUENTIAL CHAINS: [chains]
IMPLEMENTER COUNT: [number]

CONFIDENCE: [1-5]
  1 = major uncertainty, user should review
  2 = some concerns
  3 = reasonable but debatable
  4 = high confidence
  5 = trivial / obvious split

CONFIDENCE NOTES: [explain if < 4]
```

While waiting for the arbitrator to return, poll `TaskList`
every ~60 seconds and print a brief status line (e.g.
"Waiting for arbitrator... 2m elapsed").

When the arbitrator returns, print:
```
Arbitration round 1 complete.  Confidence: {score}/5.
{If >= 4: "Breakdown accepted."}
{If < 4: "Confidence below threshold — sending feedback
to planners for revision..."}
```

**Step 2a — Revision loop (max 2 retries)**

If the arbitrator's CONFIDENCE is < 4, loop up to 2 more
times (3 total rounds).  Track the round number starting
from 2.

For each retry:

Print status:
```
Re-arbitration round {N}: planners revising...
```

1. Spawn both planners again in parallel, but this time their
   prompt includes the arbitrator's full output as feedback:

```
You are a task-breakdown planner.  You previously proposed
a breakdown that was arbitrated and found to have issues.

Project path: {PROJECT}
Requirements:
{REQUIREMENTS}

Your previous breakdown:
{PLANNER_N_PREVIOUS_OUTPUT}

Arbitrator's findings:
{ARBITRATOR_OUTPUT}

Instructions:
1. Read the arbitrator's DISAGREEMENTS, KAIZEN FINDINGS,
   and CONFIDENCE NOTES carefully.
2. Address every issue the arbitrator raised.  If you
   disagree with the arbitrator on a specific point, you
   must provide new code evidence (file paths, line
   numbers) — do not simply reassert your previous
   position.
3. Re-read any code sections the arbitrator flagged.
4. Produce a revised breakdown in the same format as
   before.

Output this exact structure:

REVISION NOTES:
- [what you changed and why, referencing arbitrator's
  findings]

TASKS:
- Task 1: [description] → implementer-1
  Files: [list of affected files]
  Independent: yes/no (reason citing code)
...

PARALLEL GROUPS: [[task numbers that can run
simultaneously]]
SEQUENTIAL CHAINS: [task A → task B where B depends on A]
IMPLEMENTER COUNT: [number]
```

When planners return, print:
```
Planners revised.  Re-arbitrating...
```

2. Spawn the arbitrator again.  The arbitrator prompt is the
   same as Step 2, but with an additional preamble prepended:

```
This is re-arbitration round {N}.  The previous round
scored CONFIDENCE {score}.  The planners have revised
their breakdowns based on your feedback.  Hold them to
account — verify they actually addressed your findings,
not just rephrased them.

Previous arbitration output:
{PREVIOUS_ARBITRATOR_OUTPUT}
```

When the arbitrator returns, print:
```
Re-arbitration round {N} complete.  Confidence: {score}/5.
{If >= 4: "Breakdown accepted after {N} round(s)."}
{If < 4 and retries remain: "Still below threshold —
retrying..."}
{If < 4 and no retries remain: "Max rounds reached.
Proceeding with best available breakdown."}
```

3. If the new CONFIDENCE is >= 4, exit the loop.
4. If still < 4 after 2 retries (3 total rounds), exit the
   loop anyway — the breakdown proceeds with the best
   available result and the low confidence is surfaced.

**Step 3 — Print breakdown**

Print the full breakdown as plain output (no interaction
required) and proceed to team creation:

```
Task breakdown (arbitrated from 2 independent analyses,
{N} round(s)):

Requirements:
1. {original requirement text}
2. {original requirement text}
...

Tasks:
- Task 1: {description}
  Fulfils: Requirement(s) {N, M}
  Files: {file list}
  Rationale: {why this grouping meets the requirements}
  Assigned to: implementer-{N}

- Task 2: {description}
  Fulfils: Requirement(s) {N}
  Files: {file list}
  Rationale: {why this grouping meets the requirements}
  Assigned to: implementer-{N}

Execution plan:
  Group A (parallel): Tasks 1, 2
  Group B (sequential, after A): Task 3

Reviewer: 1 adversarial reviewer (spawned fresh per task)
Visual verification: {URL or "none"}
{If re-arbitration occurred: "Re-arbitrated {N-1} time(s).
Issues resolved: [summary]"}
{If kaizen analyses found issues:
"Kaizen analysis found: [brief summary]"}
{If confidence < 4: "⚠ Arbitrator flagged concerns:
[notes].  Will escalate if issues surface during
implementation."}

Confidence: {score}/5
Proceeding to implementation design.
```

### Phase 4: Implementation Design

This phase catches architectural inconsistencies *before* code
is written.  Per-task designers produce structured designs, then
two independent review layers — requirements validation and
cross-task consistency — verify the designs in parallel.

**Step 1 — Skip heuristic**

Not every task needs a design agent.  Skip a task if ALL of:
- It touches exactly 1 file, AND
- That file appears in no other task's file list, AND
- The task is `Independent: yes`

If all tasks are skipped, skip the entire phase.  Print:
```
Phase 4: All tasks trivial — skipping implementation design.
```

For tasks that are not skipped, print:
```
Phase 4: Spawning {N} designer agent(s) for non-trivial
tasks...
```

**Step 2 — Spawn designer agents in parallel**

Use the `Task` tool once per non-trivial task, all in a single
message, each with `subagent_type: "general-purpose"`.

Each designer's prompt:

```
You are an implementation designer.  Your job is to read the
codebase and produce a structured design for a single task,
so the implementer can execute without guesswork and without
conflicting with other parallel implementers.

Project path: {PROJECT}

Full arbitrated breakdown (all tasks, for cross-task context):
{FULL_BREAKDOWN}

Your specific task:
{TASK_N_DESCRIPTION}
Affected files: {TASK_N_FILES}

Instructions:
1. Read the affected files and their immediate dependencies
   at {PROJECT}.
2. Read files shared with other tasks (see TOUCH POINTS).
3. Produce a design covering ONLY these sections — no other
   sections, no prose outside these headings:

REUSE:
- {file_path}: {function/type/pattern} — {what it does, why
  reuse it}

CREATE:
- {entity_name}: {exact signature or type shape} — {purpose}
  (no implementation bodies)

MODIFY:
- {file_path}: {current_signature} → {proposed_signature}
  Reason: {why}

DATA FLOW:
{One paragraph: entry point → transforms → exit point.
Name every function/type in the chain.}

CONVENTIONS:
- Error handling: {pattern observed in codebase, with
  file:line example}
- Naming: {pattern observed}
- Test patterns: {pattern observed}

TOUCH POINTS:
- Task {M}: shares {file_path} — {what each task does to it}

Explicitly excluded: no pseudocode, no alternative approaches,
no test strategy.
```

While waiting for designers to return, poll `TaskList` every
~60 seconds and print a brief status line (e.g.
"Waiting for designers... 1m elapsed").

When all designers return, print:
```
{N} designer(s) returned.  Spawning review layers...
```

**Step 3 — Spawn requirements validators and consistency
reviewer in parallel**

These are two independent review layers.  Spawn all of them in
a single message.

**3a: Requirements validators (per-task, parallel)**

One validator agent per designed task, each with
`subagent_type: "general-purpose"`.

Each validator's prompt:

```
You are a requirements validator.  Your job is to verify that
a task's implementation design will actually produce a result
that meets the original requirement.

Project path: {PROJECT}

Original requirement:
{REQUIREMENT_TEXT}

Designer's output:
{DESIGNER_N_OUTPUT}

Instructions:

1. COVERAGE — decompose the requirement into every individual
   verifiable claim (the same technique used by Phase 7
   reviewers).  For each claim, identify which part of the
   design (REUSE, CREATE, MODIFY, or DATA FLOW) addresses it.
   Any claim not addressed = gap.

2. FEASIBILITY — read the actual code referenced in REUSE and
   MODIFY sections at {PROJECT}.  Verify the functions and
   types exist as described.  Check that proposed modifications
   are compatible with callers/dependents.

3. COMPLETENESS — does the design cover edge cases implied by
   the requirement?  (e.g. error states, empty inputs, auth
   boundaries)

Output this exact structure:

REQUIREMENT: {original text}

CLAIMS:
- {claim 1}: COVERED by {design section + detail}
- {claim 2}: GAP — {what's missing}
- {claim 3}: COVERED by {design section + detail}

FEASIBILITY:
- {file:function} exists: yes/no
- {proposed modification} compatible with callers: yes/no
  {if no: which callers break and why}

EDGE CASES:
- {case}: addressed/not addressed

VERDICT: MEETS_REQUIREMENTS / GAPS_FOUND
GAPS: {list of specific gaps, if any}
```

**3b: Adversarial consistency reviewer (cross-task)**

Spawn a single agent with `subagent_type: "general-purpose"`.

The consistency reviewer's prompt:

```
You are an adversarial consistency reviewer.  Your job is to
find conflicts and incompatibilities across multiple parallel
implementation designs.

Project path: {PROJECT}

All designs:
{ALL_DESIGNER_OUTPUTS}

Instructions:

Run a structured conflict hunt across all designs, checking
these six categories:

1. Signature conflicts — does any task's MODIFY contradict
   another task's REUSE of the same function/type?
2. Creation collisions — do two tasks CREATE the same entity
   or file?
3. Convention divergence — do tasks choose different error
   handling patterns, naming conventions, or test patterns?
4. Data flow incompatibility — sync/async mismatches, type
   mismatches at shared boundaries, different serialisation
   assumptions?
5. Ordering conflicts — do two tasks MODIFY the same file in
   positions that would conflict (e.g. both inserting at the
   same location)?
6. Missing touch points — do any tasks share files that
   neither acknowledges in TOUCH POINTS?

KEY REQUIREMENT: For every pair of tasks that share files,
you MUST produce an explicit NO-CONFLICT VERIFICATION or a
CONFLICT entry.  This forces enumeration of every pair —
do not skip any.

Output this exact structure:

PAIR VERIFICATIONS:
- Task {A} × Task {B}:
  Shared files: {list}
  Verification: NO CONFLICT / CONFLICT
  {if conflict: category, evidence, resolution instructions}

CONFLICTS:
- Conflict {N}: {category}
  Tasks: {A}, {B}
  Evidence: {specific signatures, types, or patterns that
  clash}
  Resolution: {specific instructions for which designer
  should change what}

VERDICT: CONSISTENT / CONFLICTS_FOUND
CONFIDENCE: {1-5}
```

While waiting for validators and consistency reviewer to
return, poll `TaskList` every ~60 seconds and print a brief
status line (e.g.  "Waiting for design review... 2m elapsed").

When all return, print:
```
Design review complete.
- Requirements validation: {N} MEETS_REQUIREMENTS,
  {M} GAPS_FOUND
- Consistency: {CONSISTENT or CONFLICTS_FOUND}
  (confidence: {score}/5)
```

**Step 4 — Retry (max 1 round)**

If either review layer found issues (`GAPS_FOUND` or
`CONFLICTS_FOUND`): re-spawn only the affected designers
with the specific findings.

Print:
```
Design issues found — re-running {K} affected designer(s)...
```

Each affected designer's retry prompt:

```
You are an implementation designer.  Your previous design was
reviewed and issues were found.  Revise your design to address
them.

Project path: {PROJECT}

Your previous design:
{DESIGNER_N_PREVIOUS_OUTPUT}

Review findings for your task:
{VALIDATOR_N_OUTPUT if GAPS_FOUND}
{CONSISTENCY_CONFLICTS affecting this task, if any}

Instructions:
1. Address every gap and conflict listed above.
2. Re-read the code if the reviewer found feasibility issues.
3. Produce a revised design in the same format (REUSE, CREATE,
   MODIFY, DATA FLOW, CONVENTIONS, TOUCH POINTS).
4. Add a REVISION NOTES section at the top listing what you
   changed and why.
```

After affected designers return, re-run both review layers
(requirements validators for revised tasks, and the
consistency reviewer across all designs including revised
ones).

Print:
```
Design retry complete.
- Requirements validation: {N} MEETS_REQUIREMENTS,
  {M} GAPS_FOUND
- Consistency: {CONSISTENT or CONFLICTS_FOUND}
```

If still failing after round 2, attach unresolved issues as
warnings to task descriptions.  Print:
```
⚠ Unresolved design issues after 2 rounds — attaching as
warnings to task descriptions.
```

One retry (not two like Phase 3) because gaps and conflicts
are factual — once identified, the fix is mechanical.

**Step 5 — Attach designs to task descriptions**

Store each design for interpolation into Phase 5 task
descriptions.

Tasks that were skipped by the skip heuristic get a minimal
inline design:
```
Follow existing patterns in {file}.  No cross-task
coordination needed.
```

Print:
```
Implementation designs finalised.  Proceeding to team
creation.
```

### Phase 5: Create the Team

Print status:
```
Creating team and tasks...
```

1. Use `TeamCreate` with name `adversarial-{timestamp}` (e.g.
   `adversarial-1708444800`).  Store team name as `{TEAM}`.

2. Use `TaskCreate` for each implementation task.  Each task
   description must include:
   - The exact original requirement text
   - The project path `{PROJECT}`
   - The affected files from the arbitrated breakdown
   - If `{URL}` is set: the URL for visual verification context
   - The implementation design section (from Phase 4):

   ```
   ## Implementation Design (follow this)

   {designer output for this task}

   Cross-task constraints:
   {consistency reviewer findings affecting this task,
   or "None" if no conflicts}

   You MUST follow the signatures, patterns, and conventions
   specified above.  If you believe the design is wrong after
   reading the code, message the team lead BEFORE deviating.
   Do not silently choose a different approach.
   ```

3. Use `TaskCreate` for each review task (one per implementation
   task).  Each review task:
   - References the corresponding requirement
   - Uses `addBlockedBy` pointing to the corresponding
     implementation task ID

4. Use `TaskUpdate` to pre-assign implementation tasks to named
   implementers per the arbitrated breakdown (set `owner` to
   `implementer-N`).

5. For sequential chains from the breakdown, use `addBlockedBy`
   on the dependent implementation tasks so they execute in
   order.

### Phase 6: Spawn Teammates

Print status:
```
Spawning {X} implementers...
```

Reviewer agents are spawned per-task in Phase 7, not here.

Spawn all implementers in a single message using multiple
`Task` tool calls in parallel.

#### Each Implementer

`subagent_type: "general-purpose"`,
`team_name: "{TEAM}"`,
`name: "implementer-N"`

Each implementer's prompt:

```
You are an implementer on an adversarial team.  Your work will
be torn apart by a dedicated reviewer — implement to a high
standard.

Project path: {PROJECT}

Your task description includes an Implementation Design
section.  This was produced by a design agent and verified
for cross-task consistency.  Follow it.  If you find it is
incorrect or incomplete after reading the code, message the
team lead before deviating — do not silently choose a
different approach.

Workflow for each task:
1. Check TaskList for tasks owned by you.
2. Claim the first available task (TaskUpdate with status:
   "in_progress").
3. Read and understand the relevant code.
4. Implement the change.
5. Lint: run the project's linter on the files you changed
   (e.g. `cargo clippy` for Rust, `npm run lint` for JS/TS).
   Fix any errors or warnings before proceeding.
6. Run tests you added: if you wrote or modified unit tests,
   run them (e.g. `cargo test -p {crate}`, `npm test --
   {file}`).  Only run tests relevant to your changes — not
   the full suite.  Fix any failures before proceeding.
7. Self-check: invoke /do-and-judge on your implementation.  The
   judge must give a VERDICT of PASS (score >= 4.0/5.0) before
   you proceed.
8. If VERDICT is FAIL: fix the issues identified and re-run
   /do-and-judge.
9. If /do-and-judge fails twice on the same task (score < 4.0
   both times), message the reviewer to discuss the requirement
   before retrying.  Wait for their response.
10. Only after VERDICT: PASS — mark the task as completed
    (TaskUpdate with status: "completed").
11. Check TaskList for your next task.  Repeat.

Rules:
- NEVER mark a task complete without a passing /do-and-judge
  verdict (SCORE >= 4.0/5.0).
- NEVER mark a task complete if linting fails or if tests you
  added are failing.
- Work only on tasks assigned to you.
- After all your tasks are done, notify the team lead.
- If during implementation you identify a clear improvement
  (e.g. a bug, dead code, missing error handling) in the files
  you're already touching, make the fix inline.  Do not suggest
  improvements — implement them.  Include them in your
  /do-and-judge self-check.  Do not scope-creep into unrelated
  files.
```

### Phase 7: Monitor and Shutdown

After spawning, report to the user:

```
Team is running:
- {X} implementers working on {N} tasks
- Reviewers: spawned fresh per review task
- Visual verification: {URL or "disabled"}

Task breakdown was arbitrated from 2 independent codebase
analyses.  Status updates every ~60s.
```

Then let the team run.  Your responsibilities:

1. **Periodic status polling**: every ~60 seconds, check
   `TaskList` and print a summary:
   ```
   Status update ({elapsed} elapsed):
   - implementer-1: Task 2 "Add auth middleware" — in_progress
   - implementer-2: Task 1 "Create settings page" — completed
   - reviewer (task 1): Review 1 — pending (blocked by Task 2)
   ```

2. **Auto-nudge idle agents**: during each polling cycle, check
   for tasks that are unblocked (no pending `blockedBy`) but
   still `pending` with an assigned owner, or unblocked but
   unclaimed.  If found, send a message to the relevant
   teammate: "Task N is now unblocked and ready for you."  For
   unclaimed tasks, assign them to an available implementer and
   nudge.

3. **Spawn fresh reviewer per review task**: when a review task
   becomes unblocked (its implementation task completed), spawn
   a fresh `Task` agent with `subagent_type: "general-purpose"`,
   `team_name: "{TEAM}"`, and `name: "reviewer-task-{N}"`.

   The reviewer prompt includes:

   ```
   You are the adversarial reviewer for a single task.  Your
   job is to find flaws.  You are not here to help the
   implementer — you are here to catch problems.  Be thorough
   and adversarial.

   Project path: {PROJECT}
   Visual verification URL: {URL or "none"}

   Reference images (from user's original requirements):
   {REFERENCE_IMAGES or "none"}

   If reference images are listed above, Read each file and
   compare the implementation against them.  These are the
   user's design intent — use them as the ground truth for
   visual correctness.

   Original requirement:
   {requirement text for this task}

   Task description: {task description}
   Affected files: {file list from breakdown}

   Your review task ID: {review task ID}

   Print status before each step so the user can see
   progress in your terminal.

   Workflow:
   1. Claim the review task (TaskUpdate with status:
      "in_progress", owner: "reviewer-task-{N}").
      Print: "Reviewing task {N}: {short requirement summary}"

   2. Read the code changes: use git diff or read the modified
      files directly.
      Print: "Code review complete — read {count} files"

   3. Run relevant tests: run the tests for the areas affected
      by this task (e.g. `cargo test -p {crate}`, `npm test --
      {path}`).  Only run tests relevant to the affected files
      — not the full suite.  Record the results: pass count,
      fail count, any failures.
      Print: "Tests: {pass} passed, {fail} failed"

   4. Requirements checklist: decompose the original
      requirement into every individual, verifiable claim
      (e.g. "text on one line", "uses auth middleware",
      "returns 404 on missing ID").  For each claim:
      a. State the claim.
      b. Find the specific code (file:line) or output
         that satisfies it.
      c. Mark it: MET (with evidence) or NOT MET (with
         what you found instead).
      Print the checklist:
      "Requirements checklist:
       ✅ {claim} — {file:line or evidence}
       ❌ {claim} — found: {what's actually there}
       ..."
      If ANY claim is NOT MET, you must FAIL the review
      in your final verdict — do not defer to /critique
      to catch it.

   5. If a visual verification URL is set:
      Print: "Navigating to {URL} for visual verification..."

      a. Ensure the output directory exists and verify the
         server is running.  Run via Bash:
         ```
         mkdir -p {PROJECT}/.adversarial-review && \
         curl -s -o /dev/null -w "%{http_code}" {URL}
         ```
         If the status is not 200, print:
         "Server not responding at {URL} (HTTP {code})"
         and skip visual verification.  Note this in the
         review report as a blocker.

      b. Take screenshots at 1280px and 768px viewport widths.
         Try each method in order until one succeeds:

         Method 1 — puppeteer via npx (most reliable):
         ```
         cd {PROJECT} && npx puppeteer screenshot {URL} \
           --viewport 1280x720 --full-page \
           --output .adversarial-review/screenshot-1280.png
         ```

         Method 2 — playwright CLI:
         ```
         npx -y playwright screenshot {URL} \
           --viewport-size=1280,720 --full-page \
           {PROJECT}/.adversarial-review/screenshot-1280.png
         ```

         Method 3 — inline node script (if puppeteer is a
         project dependency):
         ```
         cd {PROJECT} && node -e "
         const p = require('puppeteer');
         (async () => {
           const b = await p.launch({
             headless: 'new',
             args: ['--no-sandbox']
           });
           const pg = await b.newPage();
           await pg.setViewport({width:1280, height:720});
           await pg.goto('${URL}', {waitUntil:'networkidle2'});
           await pg.screenshot({
             path:'.adversarial-review/screenshot-1280.png',
             fullPage:true
           });
           await b.close();
         })();
         "
         ```

         Once a method works, use the same method for the
         768px screenshot (change width to 768, filename to
         screenshot-768.png).

         If ALL methods fail, print the error and note in
         the review report:
         "Visual screenshots FAILED — no browser automation
         tool available.  Install puppeteer or playwright."
         Continue the review without screenshots but flag
         this as a gap in the Visual Verification section.

         Print after each successful screenshot:
         "Screenshot captured: {width}px viewport"

      d. Read the screenshot image files using the Read tool.
         Compare both screenshots against the original
         requirement.
         Print: "Visual comparison against requirement complete"

      e. If reference images exist, for each image:
         Print: "Reading reference image: {path}"
         Read the image file.
         Print: "Comparing reference image against live
         screenshot..."
         Compare the reference image against the live
         screenshots.

   6. Run /critique on the changes to get a multi-judge
      assessment.
      Print before: "Running /critique..."
      Print after: "Critique complete — verdict: {verdict}"

   7. Write your review report.  The report MUST contain ALL
      of the following sections regardless of PASS or FAIL.
      Missing sections = incomplete review = you must redo it.
      Print: "Writing review report..."

      ## Review Report: Task {N}

      ### Requirement
      Quote the original requirement text verbatim.

      ### Code Review Findings
      List each file you read.  For each, note what changed
      and whether it addresses the requirement.  If you found
      edge cases, regressions, or issues, list them.

      ### Requirements Checklist
      For each verifiable claim extracted from the
      requirement, state the claim, the evidence
      (file:line), and whether it is MET or NOT MET.

      ### Test Results
      List the test command(s) run, pass/fail counts, and
      any failure details.

      ### /critique Results
      Paste the /critique verdict AND the key points from
      each judge.  Do not summarise — include the actual
      output.

      ### Visual Verification (if URL or reference images)
      For each screenshot taken, describe:
      - The viewport width
      - What you observed on screen
      - How it compares to the requirement / reference image
      If reference images were provided, explicitly state
      what matches and what differs.

      ### Verdict: PASS or FAIL
      State your verdict.  If FAIL, list each specific gap
      between requirement and implementation.

      Print: "Verdict: PASS ✓" or
             "Verdict: FAIL ✗ — {count} issue(s)"

   8. Send the full review report to the team lead via
      SendMessage.

   9. Then update the task:
      - PASS → mark review task completed.
      - FAIL → message the implementer with the specific
        issues from your report.  Notify the team lead that
        a re-review will be needed after the fix.

   Rules:
   - NEVER approve without both code review AND running
     /critique.
   - NEVER approve if /critique verdict is "Requires
     significant rework".
   - NEVER approve if any item in the requirements
     checklist is NOT MET.
   - NEVER approve if relevant tests are failing.
   - If a visual verification URL is set: NEVER approve
     without visual verification via screenshots at both
     viewport widths.
   - Be adversarial — actively try to break the
     implementation, check edge cases, check for regressions
     in surrounding code.
   - Only screenshot {URL} — no other URLs.
   - If an implementer messages you to discuss a requirement
     (because their /do-and-judge failed twice), engage
     genuinely — help clarify, but do not lower your review
     standards.
   - Your review report is your proof of work.  Every claim
     must be backed by specifics: file paths, line numbers,
     screenshot descriptions, /critique output.  "Looks good"
     is never sufficient.
   - If reference images were provided, you MUST Read them
     and compare against the live screenshots.  State what
     matches and what does not.
   ```

   When a reviewer sends you its review report:

   **Validate the report before accepting PASS.**  Check that
   it contains all required sections: Requirement, Code Review
   Findings, Requirements Checklist, Test Results, /critique
   Results, Visual
   Verification (if URL or reference images were provided),
   and Verdict.

   If any section is missing or says something like "verified"
   without specifics, message the reviewer:  "Your review
   report is incomplete — section {X} is missing or lacks
   detail.  Please redo the review."

   Only count the review task as successfully completed once
   the report passes validation.

   If a reviewer FAILs the implementation, wait for the
   implementer to fix it, then spawn a **new** fresh reviewer
   agent for the re-review (do not reuse the previous one).
   Track the review cycle count per task.

4. **Respond to teammate messages** as they arrive.

5. **Deadlock detection**: if a task goes through 3+ review
   cycles (3 fresh reviewer spawns), intervene.  Use
   `AskUserQuestion` to ask the user for guidance on the
   contested requirement.

6. **When all review tasks are completed**:
   a. Send `shutdown_request` to every active teammate.
   b. Wait for all shutdown confirmations.
   c. Use `TeamDelete` to clean up.
   d. Present a final summary to the user:

```
All tasks complete.

Summary:
- Tasks completed: {N}
- Review cycles per task:
  - Task 1 ({description}): {cycles} cycle(s)
  - Task 2 ({description}): {cycles} cycle(s)
  ...
- Issues escalated to user: {count or "none"}
- Visual verification: {performed/skipped}
- Inline improvements by implementers: {count or "none"}
```

## Notes

- Phase 3 (adversarial task breakdown) is not optional, even for
  a single requirement.  The two-planner + arbitrator pattern
  catches coupling that a single analysis might miss.
- The re-arbitration loop (Step 2a) runs at most 2 retries
  (3 total rounds).  If confidence remains < 4 after all rounds,
  the breakdown proceeds with a warning.
- Phase 4 (implementation design) has a skip heuristic for
  trivial tasks (single-file, independent, no shared files).
  The design retry loop runs at most 1 retry (2 total rounds)
  because gaps and conflicts are factual, not ambiguous.
  Requirements validators and the consistency reviewer run in
  parallel since they address independent concerns.
- The breakdown is always printed and execution proceeds
  automatically.  Deadlock escalation (Phase 7) still uses
  `AskUserQuestion`.
- `/do-and-judge` is the first adversarial layer (implementer
  self-check).  Pass threshold is SCORE >= 4.0/5.0.
- `/critique` is the second adversarial layer (reviewer external
  check).  Block threshold is verdict "Requires significant
  rework".
- If `{URL}` is provided, visual verification with puppeteer
  screenshots is the third layer.
- Reviewers are spawned fresh per review task (Phase 7, not
  Phase 6).  This avoids context window bloat across multiple
  reviews.  Each reviewer gets the original requirement text,
  task description, and full instructions — no accumulated
  context from prior reviews.
- The reviewer can message implementers directly via the team —
  this enables real back-and-forth debate, not just pass/fail
  verdicts.
- Agents spawn into their own panes.  The ~60-second status
  polling keeps the orchestrator informed of progress.
- Implementers are expected to fix obvious improvements (bugs,
  dead code, missing error handling) in files they already
  touch, without scope-creeping into unrelated files.
- The reviewer must send a structured review report to the
  team lead for every verdict (PASS or FAIL).  The
  orchestrator validates report completeness before
  accepting a PASS — this prevents rubber-stamp approvals.
- If the user pastes images with their requirements, they
  are saved to `{PROJECT}/.adversarial-review/` in Phase 1
  and passed to reviewers as `{REFERENCE_IMAGES}` for
  comparison.
````
