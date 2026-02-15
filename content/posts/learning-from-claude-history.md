+++
title = "Learning From Your Claude History"
date = 2026-02-15T16:19:40Z
images = []
tags = []
categories = []
draft = false
+++

I read this article from Anthropic: ["How AI assistance impacts the formation
of coding skills"](https://www.anthropic.com/research/AI-assistance-coding-skills).
It got me a bit concerned.  Are we heading towards a crisis of understanding?

But then I thought about conversations I've had with colleagues recently.  Especially
around "is this the end for Junior Engineers?".  I don't think it is.  Mostly because
I don't think the arrival of AI assistance, Agents, into our engineering toolkit has
represented any _fundamental_ change, but instead a large leap forward in a direction
we were already heading.

---

Since the dawn of computers, we engineers have steadily climbed the level of abstraction
that we work at day to day.  Over time the opportunity to work at higher levels of 
abstraction has presented itself, and generally made programming computers more accessible
to the uninitiated and more productive for those who already can.  Think of the progression
of: Assembly, C, garbage-collected languages, languages that don't need compiling and linking,
finishing this chain at something like Python - a very accessible high level language in
which you can achieve things quickly.

Say you've got a team of some "Python engineers" - your product's mostly in Python, your
engineers mostly work in Python.  Who's the most valuable engineer on that team?  It's the
engineer you call upon when things get difficult: there's an issue with memory growth, the
service is not fast enough, there's a bug in asynchronous code that stops showing up
when you try to debug it.  And how are they able to tackle those difficult problems?  By having
an understanding of what's happening below the level of abstraction that they work at most of
the time.

The rise of coding agents that we're seeing right now, I see as a continuation of this trend.
Over the last few weeks I've been progressively trying to use AI tooling more.  And I've been asked
"how does it feel to not be writing code?".  But I feel like I am still writing code.  Sure, I'm
not typing out every character of it.  But I wasn't doing that before.  For nearly the last decade,
I and most other engineers from what I can gather, have been using LSP-powered auto-complete.  To
the extent that without it I struggle to one-shot typing valid syntax - embarrassingly so.  So
one could argue that we've already been working "abstracted away from syntax", to a certain extent,
before AI and Agents came along.  But there certainly is a further level of abstraction that
AI and Agents take you to working at, one that's a big jump of abstraction compared to what we
were working at before.

We've moved the level of abstraction which we work with to one of natural language.

And the opportunity for producing and delivering stuff that I don't really understand is
a lot higher.

---

There's a lot of predictions of the future that I hear and read going around.  A lot of them are
around who's going to be put out of a job in X months, and who's going to be the most valuable
engineers to an organisation.  I see a theme of arguments that revolve around proficiency with using
AI tooling being the key factor.

And I can see where that is coming from.  I'm pretty new to all this stuff, but I can see that you
don't immediately get great results using it out of the box without some effort learning how it
needs to be tuned and orchestrated.  Just opening up Claude Code and asking it to do your work for
you doesn't get you very far.

But I think the incentives are sufficient for that not to be the case for very long.  If one 
companies Agent requires an endless list of heuristics and tricks, with layers of "scaffolding"
written in markdown and even whole applications to orchestrate it and coerce good results out of it,
where alternatively another company's Agent gives you pretty good results out of the box - I think
it's clear where you're going to spend your money.  And we're already seeing this happen.  In the
very short period of time that I've been using Claude Code, the direct / native experience of using
it has greatly improved.  I've found myself getting rid of some of the layers I had tried using
over the top.  E.g: with recent improvements to "plan mode" I found myself uninstalling the "GSD"
framework.

What I mean by this is that I don't think the current apparent gap in productivity between those who 
are well versed in all the AI tooling and those who are new to using it is going to be as big in the
near future, as getting good results out of the tooling is going to get easier.

---

So coming back to the point here: why was I a bit worried by 
[Anthropic's article](https://www.anthropic.com/research/AI-assistance-coding-skills), then why did I
decide that I actually wasn't that worried?

How we're working as engineers is changing in a big way.  I don't think it's changing in a _fundamental_
way, as this is the same pattern that has happened before: "we're moving to work at a higher level of
abstraction", and what makes an engineer highly skilled and valuable is also staying the same: "how do
things work underneath the level of abstraction that you interact with them at?".

But this is quite a large change, and us engineers need to be deliberate in our approach.  I think
we need to be more conscious and intentional about learning how things actually work.  As we're abstracting
ourselves further from it the opportunity to learn it "by osmosis" is diminishing.  I don't think that
it's game over for Junior Engineers, but compared with my generation I think they need to take a different
path to gaining expertise.  Now that we can build stuff while working at a level so far abstracted from
"how things work", a more deliberate approach to learning is called for - it's less "on the job" than it
was before.

In these past few weeks, with the assistance of Claude Code I've been able to independently push deeper
into very unfamiliar territories.  I've written frontend code for the first time, and done some stuff
with databases that was far beyond my previous understanding of them.  But after getting something
working I'll admit I didn't really understand how they worked.

I came across the `/insights` command in Claude Code.  It essentially opens up an HTML formatted report
in your browser of all the recent activity you've had and recommendations for how you could improve
your usage.  This made me realise that there is the wealth of information sitting there in Claude -
all the things you've built with it, conversations you've had etc.

Where I've been concerned that using AI is "taking the learning away", the AI has the information on
all the technical areas you've worked on which can be used to help you learn.

I had a quick look around on the interwebs for "is there something that can pull technical learnings
out of Claude chat history?".  From that quick cursory search I couldn't find anything that I could
just grab and use.

So I worked with Claude to develop this Skill for exactly that: look at recent conversations and
either write a summary of the technical areas or start an interactive conversation with the user to
dive into specific topics.  This took some iterations to get right, it didn't work that well to begin
with, but eventually I got something I was happy with.  I made it infer the level of expertise I had
on a topic based on the questions I was asking in the conversations.

This `/learn` command will let you choose how far back in time you want to take conversations from
and whether you drop into an interactive discussion or just get a summary written to file.

I had quite a good conversation with it on Friday and felt it helped give me a bit more of an
understanding of the things I had built in the past few weeks.

---

If you use Claude Code, and want to give this a try, place the following two files into:

- `~/.claude/skills/learn/SKILL.md`
- `~/.claude/skills/learn/scripts/extract-sessions.py`

```markdown
---
name: learn
description: >
  Generate a personalised learning document from recent
  Claude Code sessions, or interactively explore topics
  covered in past work.
disable-model-invocation: true
argument-hint: "[summary|explore] [days|topic]"
---

# /learn Skill

You are generating educational content from the user's
recent Claude Code sessions.  The extraction script has
already been run and its output is included below.

## Session Data

!`python3 ~/.claude/skills/learn/scripts/extract-sessions.py --days 7`

## Determine Mode from Arguments

The user invoked `/learn` with these arguments: $ARGUMENTS

Parse them as follows:

- **No arguments** → ask the user which mode they want
  (summary or explore) and how many days back to look
  (suggest 7, 14, or 30).  Then follow the corresponding
  mode instructions below.  If the user picks a different
  timeframe than 7 days, re-run the extraction script
  with the chosen value by executing:
  `python3 ~/.claude/skills/learn/scripts/extract-sessions.py --days N`
  and use that output instead.
- **`summary`** → Summary Mode, default 7 days
- **`summary N`** (where N is a number) → Summary Mode,
  N days.  Re-run the extraction script:
  `python3 ~/.claude/skills/learn/scripts/extract-sessions.py --days N`
- **`explore`** → Explore Mode, no topic filter
- **`explore TOPIC`** → Explore Mode, filtered to TOPIC.
  Re-run the extraction script:
  `python3 ~/.claude/skills/learn/scripts/extract-sessions.py --days 30 --topic TOPIC`

## Knowledge-Level Inference

Before generating any content, analyse the user's messages
in the session data to infer their knowledge level for each
technology or concept area.  Use these signals:

**Expert signals:**
- Precise implementation directives ("use a B-tree index",
  "implement with zero-copy deserialization")
- Correct, specific terminology
- Architectural opinions and trade-off reasoning
- Telling Claude exactly what to do, not asking how

**Intermediate signals:**
- States technical requirements but delegates
  implementation details to Claude
- Asks specific, targeted questions
- Uses correct terminology but seeks confirmation

**Beginner signals:**
- Vague or high-level requests ("make it faster",
  "add a database")
- Describes desired outcome without technical detail
- Asks "what is X" or "how does X work" style questions
- Defers entirely to Claude's suggestions

Calibrate your explanations to the inferred level.  Do
not over-explain things the user clearly already knows.
Do not under-explain things where they showed uncertainty.

## Summary Mode

Generate a structured learning document.  Follow these
steps:

1. Enumerate **every** distinct technology, library,
   pattern, and concept area from the session data.
   Include all of them — do not filter, rank, or pick
   a "top N".  If the user touched it in a session,
   it gets a section.  Group closely related work into
   one topic (e.g. two sessions on the same NATS
   pattern = one topic), but do not merge unrelated
   areas just to reduce the count.
2. For each concept area, infer the user's knowledge
   level.
3. Generate a document organised **by topic** (not
   chronologically) with the following structure for
   each:
   - **What was built/done** — brief summary referencing
     the specific session work
   - **Key concepts** — explained at the inferred
     knowledge level
   - **Patterns and gotchas** — practical lessons from
     the sessions
   - **Connections** — how this topic relates to other
     topics covered
   - **Go deeper** — pointers for further learning
     (documentation links, concepts to explore next)
4. Print a concise summary in the conversation showing
   the topics covered and a one-liner for each.
5. Save the full document to a file.  Use `mkdir -p`
   to create `~/learning/` if it does not exist, then
   write the document to:
   `~/learning/YYYY-MM-DD-summary.md`
   where the date is today's date.  If that file already
   exists, append a sequence number:
   `~/learning/YYYY-MM-DD-summary-2.md`,
   `~/learning/YYYY-MM-DD-summary-3.md`, etc.

Keep the document practical and grounded in the user's
actual work — not generic textbook material.

## Explore Mode

Enter an interactive topic exploration loop:

1. From the session data, identify distinct
   technology/concept areas.
2. Present a **numbered list** of topics with a one-line
   description of what was done in each.
3. If a topic was provided in the arguments, skip the
   list and jump straight to step 4 for that topic.
4. When the user picks a topic (by number or name):
   - Give a calibrated explanation of the core concepts,
     referencing specific things from their sessions
   - Highlight what they did well and areas that could
     be explored further
   - Invite follow-up questions
5. Continue the Q&A naturally.  The user can ask about
   the current topic, switch topics, or end the session.

## Style Guidelines

- Use the Oxford comma, and put a double space after
  full stops.
- Keep markdown line length to 80 characters or less
  (except code blocks).
- Be direct and practical.  Avoid filler phrases.
- Reference the user's actual code and sessions, not
  generic examples.
- When the user is expert-level in a topic, focus on
  nuances, edge cases, and advanced patterns rather
  than basics.
```

```python
#!/usr/bin/env python3
"""Extract recent Claude Code session data into compact markdown.

Scans ~/.claude/projects/*/*.jsonl for session files modified within
the last N days, parses user/assistant exchanges, and outputs
structured markdown suitable for learning review.

No external dependencies — stdlib only.
"""

import argparse
import json
import sys
from datetime import datetime, timedelta, timezone
from pathlib import Path

CLAUDE_DIR = Path.home() / ".claude"
PROJECTS_DIR = CLAUDE_DIR / "projects"
HISTORY_FILE = CLAUDE_DIR / "history.jsonl"

MAX_USER_MSG = 1000
MAX_ASSISTANT_MSG = 500
MAX_EXCHANGES_PER_SESSION = 8
MAX_SESSIONS_PER_PROJECT = 15
MAX_OUTPUT_BYTES = 50_000


def parse_args():
    parser = argparse.ArgumentParser(
        description="Extract recent Claude Code sessions as markdown"
    )
    parser.add_argument(
        "--days",
        type=int,
        default=7,
        help="How far back to look (default: 7)",
    )
    parser.add_argument(
        "--topic",
        type=str,
        default=None,
        help="Filter sessions to those mentioning this keyword",
    )
    return parser.parse_args()


def load_history_summaries():
    """Load session summaries from history.jsonl keyed by sessionId."""
    summaries = {}
    if not HISTORY_FILE.exists():
        return summaries
    with open(HISTORY_FILE, "r") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            try:
                entry = json.loads(line)
            except json.JSONDecodeError:
                continue
            sid = entry.get("sessionId")
            display = entry.get("display", "")
            if sid and display:
                summaries[sid] = display
    return summaries


def find_session_files(cutoff):
    """Find .jsonl session files modified after cutoff datetime."""
    if not PROJECTS_DIR.exists():
        return []

    files = []
    for jsonl in PROJECTS_DIR.glob("*/*.jsonl"):
        try:
            mtime = datetime.fromtimestamp(
                jsonl.stat().st_mtime, tz=timezone.utc
            )
        except OSError:
            continue
        if mtime >= cutoff:
            files.append(jsonl)
    return files


def extract_text_from_content(content):
    """Pull plain text from a message content field."""
    if isinstance(content, str):
        return content
    if isinstance(content, list):
        parts = []
        for block in content:
            if isinstance(block, str):
                parts.append(block)
            elif isinstance(block, dict) and block.get("type") == "text":
                parts.append(block.get("text", ""))
        return "\n".join(parts)
    return ""


def has_tool_use(content):
    """Check if content contains a tool_use block."""
    if isinstance(content, list):
        for block in content:
            if isinstance(block, dict) and block.get("type") == "tool_use":
                return True
    return False


def parse_session(filepath):
    """Parse a session JSONL file into structured data.

    Returns a dict with metadata and a list of exchanges, or None
    if the file has no usable content.
    """
    session_id = filepath.stem
    project_dir = filepath.parent.name
    cwd = None
    git_branch = None
    timestamp = None
    exchanges = []

    with open(filepath, "r") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            try:
                entry = json.loads(line)
            except json.JSONDecodeError:
                continue

            # Grab metadata from system entries
            if entry.get("type") in ("system", "user", "assistant"):
                if cwd is None and entry.get("cwd"):
                    cwd = entry["cwd"]
                if git_branch is None and entry.get("gitBranch"):
                    git_branch = entry["gitBranch"]
                if timestamp is None and entry.get("timestamp"):
                    timestamp = entry["timestamp"]

            if "message" not in entry:
                continue

            msg = entry["message"]
            role = msg.get("role")
            content = msg.get("content", "")

            if role == "user":
                # Skip tool_result messages
                if isinstance(content, list):
                    is_tool_result = any(
                        isinstance(b, dict) and b.get("type") == "tool_result"
                        for b in content
                    )
                    if is_tool_result:
                        continue

                text = extract_text_from_content(content)
                text = text.strip()
                # Skip system/command messages
                if text.startswith("<local-command") or text.startswith(
                    "<command-name"
                ):
                    continue
                if text:
                    exchanges.append(("user", text[:MAX_USER_MSG]))

            elif role == "assistant":
                # Skip messages that are only tool_use
                if has_tool_use(content):
                    text = extract_text_from_content(content)
                    text = text.strip()
                    if not text:
                        continue
                else:
                    text = extract_text_from_content(content)
                    text = text.strip()

                if text:
                    exchanges.append(
                        ("assistant", text[:MAX_ASSISTANT_MSG])
                    )

    if not exchanges:
        return None

    return {
        "session_id": session_id,
        "project_dir": project_dir,
        "cwd": cwd,
        "git_branch": git_branch,
        "timestamp": timestamp,
        "exchanges": exchanges,
    }


def matches_topic(session, topic):
    """Check if any exchange in the session mentions the topic."""
    topic_lower = topic.lower()
    for _, text in session["exchanges"]:
        if topic_lower in text.lower():
            return True
    return False


def project_label(project_dir):
    """Convert a project dir name to a human-readable label.

    e.g. '-home-tom-code-bonkbot-pum3' -> 'bonkbot/pum3'
    """
    parts = project_dir.strip("-").split("-")
    # Drop the home/user prefix
    try:
        home_idx = parts.index("home")
        # Skip home, username, and 'code' if present
        start = home_idx + 2
        if start < len(parts) and parts[start] == "code":
            start += 1
        parts = parts[start:]
    except ValueError:
        pass
    return "/".join(parts) if parts else project_dir


def format_date(ts_str):
    """Extract a YYYY-MM-DD date from an ISO timestamp string."""
    if not ts_str:
        return "unknown date"
    try:
        dt = datetime.fromisoformat(ts_str.replace("Z", "+00:00"))
        return dt.strftime("%Y-%m-%d")
    except (ValueError, AttributeError):
        return "unknown date"


def sort_projects(sessions_by_project):
    """Sort projects by most recent session timestamp, descending."""
    def key(item):
        _dir, sessions = item
        return sessions[0].get("timestamp") or ""

    return sorted(
        sessions_by_project.items(), key=key, reverse=True
    )


def render_session_header(session, history_summaries):
    """Render a compact session header (no exchanges)."""
    date = format_date(session["timestamp"])
    sid = session["session_id"]
    summary = history_summaries.get(sid, "")
    branch = session.get("git_branch", "")
    first_user = ""
    for role, text in session["exchanges"]:
        if role == "user":
            first_user = " ".join(text.split())[:300]
            break

    lines = []
    header = f"### Session: {date} ({sid[:8]})"
    lines.append(header)
    if summary:
        lines.append(f'Summary: "{summary}"')
    if branch:
        lines.append(f"Branch: `{branch}`")
    if first_user:
        lines.append(f"**User:** {first_user}")
    lines.append("")
    lines.append("---")
    lines.append("")
    return "\n".join(lines)


def render_session_full(session, history_summaries):
    """Render a session with exchanges."""
    date = format_date(session["timestamp"])
    sid = session["session_id"]
    summary = history_summaries.get(sid, "")
    branch = session.get("git_branch", "")

    lines = []
    header = f"### Session: {date} ({sid[:8]})"
    lines.append(header)
    if summary:
        lines.append(f'Summary: "{summary}"')
    if branch:
        lines.append(f"Branch: `{branch}`")
    lines.append("")

    exchanges = session["exchanges"]
    for role, text in exchanges[:MAX_EXCHANGES_PER_SESSION]:
        compact = " ".join(text.split())
        if role == "user":
            lines.append(f"**User:** {compact}")
        else:
            lines.append(f"**Assistant:** {compact}")
        lines.append("")

    if len(exchanges) > MAX_EXCHANGES_PER_SESSION:
        omitted = len(exchanges) - MAX_EXCHANGES_PER_SESSION
        lines.append(f"*({omitted} more exchanges omitted)*")
        lines.append("")

    lines.append("---")
    lines.append("")
    return "\n".join(lines)


def render_markdown(sessions_by_project, history_summaries, days):
    """Render sessions as structured markdown.

    Uses a two-pass approach to guarantee every project and
    session appears in the output:
      Pass 1 — compact headers (project + session summaries)
      Pass 2 — fill in exchange detail per project budget
    """
    now = datetime.now(timezone.utc)
    start = now - timedelta(days=days)

    doc_header = (
        f"# Session History: "
        f"{start.strftime('%Y-%m-%d')} — {now.strftime('%Y-%m-%d')}"
        "\n\n"
    )

    sorted_projects = sort_projects(sessions_by_project)

    # Pass 1: render compact headers for ALL projects/sessions.
    # This guarantees every project is visible to the LLM.
    pass1_parts = [doc_header]
    for project_dir, sessions in sorted_projects:
        label = project_label(project_dir)
        cwd = sessions[0].get("cwd", "")
        proj_header = f"## Project: {label}\n"
        if cwd:
            proj_header += f"Path: {cwd}\n"
        proj_header += "\n"
        pass1_parts.append(proj_header)

        for session in sessions:
            pass1_parts.append(
                render_session_header(session, history_summaries)
            )

    pass1_output = "".join(pass1_parts)
    pass1_size = len(pass1_output.encode("utf-8"))

    # If headers alone exceed the budget, return them truncated.
    if pass1_size >= MAX_OUTPUT_BYTES:
        return pass1_output

    # Pass 2: replace compact headers with full exchanges,
    # distributing remaining budget fairly across projects.
    remaining = MAX_OUTPUT_BYTES - pass1_size
    n_projects = len(sorted_projects)
    per_project_budget = remaining // max(n_projects, 1)

    output_parts = [doc_header]
    surplus = 0

    for project_dir, sessions in sorted_projects:
        label = project_label(project_dir)
        cwd = sessions[0].get("cwd", "")
        proj_header = f"## Project: {label}\n"
        if cwd:
            proj_header += f"Path: {cwd}\n"
        proj_header += "\n"

        allowance = per_project_budget + surplus
        project_detail = ""

        for session in sessions:
            full = render_session_full(session, history_summaries)
            compact = render_session_header(
                session, history_summaries
            )
            # Cost of upgrading this session from compact to full
            extra = (
                len(full.encode("utf-8"))
                - len(compact.encode("utf-8"))
            )
            if extra <= allowance:
                project_detail += full
                allowance -= extra
            else:
                # Budget exhausted — render remaining as compact
                project_detail += compact

        output_parts.append(proj_header + project_detail)
        surplus = max(allowance, 0)

    return "".join(output_parts)


def main():
    args = parse_args()
    cutoff = datetime.now(timezone.utc) - timedelta(days=args.days)

    history_summaries = load_history_summaries()
    session_files = find_session_files(cutoff)

    if not session_files:
        print(
            f"No sessions found in the last {args.days} day(s).",
            file=sys.stderr,
        )
        sys.exit(0)

    # NOTE: Only include sessions that appear in history.jsonl.
    # This filters out subagent sessions which are noise.
    history_session_ids = set(history_summaries.keys())

    sessions_by_project = {}
    for sf in session_files:
        # Skip files whose session ID isn't in history
        if sf.stem not in history_session_ids:
            continue
        session = parse_session(sf)
        if session is None:
            continue
        if args.topic and not matches_topic(session, args.topic):
            continue
        project = session["project_dir"]
        sessions_by_project.setdefault(project, []).append(session)

    # Sort sessions within each project by timestamp (most recent
    # first), then cap to MAX_SESSIONS_PER_PROJECT.
    for project in sessions_by_project:
        sessions_by_project[project].sort(
            key=lambda s: s.get("timestamp") or "", reverse=True
        )
        sessions_by_project[project] = (
            sessions_by_project[project][:MAX_SESSIONS_PER_PROJECT]
        )

    if not sessions_by_project:
        if args.topic:
            print(
                f"No sessions mentioning '{args.topic}' "
                f"in the last {args.days} day(s).",
                file=sys.stderr,
            )
        else:
            print(
                f"No sessions with extractable content "
                f"in the last {args.days} day(s).",
                file=sys.stderr,
            )
        sys.exit(0)

    output = render_markdown(
        sessions_by_project, history_summaries, args.days
    )

    # Enforce size cap
    if len(output.encode("utf-8")) > MAX_OUTPUT_BYTES:
        encoded = output.encode("utf-8")[:MAX_OUTPUT_BYTES]
        output = encoded.decode("utf-8", errors="ignore")
        # Trim to last complete line
        last_nl = output.rfind("\n")
        if last_nl > 0:
            output = output[:last_nl]
        output += "\n\n*(output truncated to ~50KB)*\n"

    print(output)


if __name__ == "__main__":
    main()
```
