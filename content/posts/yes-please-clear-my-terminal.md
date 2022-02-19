+++
title = "yes, please clear my terminal"
date = 2022-02-19T15:00:48Z
images = []
tags = []
categories = []
draft = true
+++

We've all been there.  You've been running some commands.  You switch focus
away from your terminal.  You come back.  And your terminal is full of crap
you're not interested in anymore.  “Get this out of my sight”, you think.
[`clear`][clear] you type.

Problem solved.  Blog post over.  Oh but wait!  This doesn't solve all my woes.

There's another time when the ghosts of a command past can muddy your view.
Say you've run some tests.  There's lots of output that you'll need to scroll
back through in case of a failure.  You find the failure.  You make a fix. You
run the tests again.  There's another failure.  So you scroll up, `Ctrl A, [,
PgUp` or however you do it.  This second time it's easy to miss where your most
recent test output starts and where the old one ends.

I found [`clear`][clear] doesn't work for me this time.  Running [`clear`][clear], then scrolling
up, I see no gap between the previous output and my prompt.  Looking into [the
man pages][clear], it says “clears
your screen if this is possible, including its scrollback buffer (if the
extended “E3” capability is defined).”.  OK so this is probably a `tmux` issue.
Exiting `tmux` I find that [`clear`][clear] does in fact clear the scroll history too.
Outside of `tmux` , passing the `-x` parameter to [`clear`][clear] (to stop it clearing
scrollback buffer) give me the same effect as I saw in `tmux` .

[clear]: https://man7.org/linux/man-pages/man1/clear.1.html

So at this point I should probably give up on this blog post and go and work
out how to make [`clear`][clear] work the same in `tmux`.  But wait, there might still
be a use case that [`clear`][clear] can't fulfil here.  Suppose I want to not throw away
the old test output, but still have a clear point in the history where the old
test output ended and a new one begins.

You could solve this problem by holding down the `return` key for a few seconds
so there was an obvious band in the terminal history of just the prompt.  But
there's a slightly more inventive solution:

```bash
yes "" | head -50
```

What's going on here? [`yes`][yes] is a
command that will repeatedly
output the string given (or `y` if no string is given). So giving it the empty
string will make it produce newlines to stdout.

[yes]: https://man7.org/linux/man-pages/man1/yes.1.html

The appearance of [`yes`][yes] in script or a command line is almost always a sign
that you're doing something wrong.  It can be used as a hack to automate
interactive CLIs that you know you're going to say “yes” to:

```bash
yes | apt install foo

yes | rm -ir some_directory/
```

Or even if you know you're going to say “no”:

```bash
yes no | <command>
```

I know, right.

But CLI commands *usually* have parameters that you can pass to achieve a
non-interactive mode:

```bash
apt install -y foo

rm -rif some_directory/
```

It's somewhat reminiscent of the [“Useless Use of
`cat`".](https://en.wikipedia.org/wiki/Cat_(Unix)#Useless_use_of_cat)

Apart from my beautiful command above, I think I've only used [`yes`][yes]
”legitimately” once: doing a quick test of a TCP connection by piping [`yes`][yes]
into netcat... maybe.  What is “legitimate usage” anyway...

`head -50` will take the first 50 lines of this.  The `-50` parameter is
*actually an [undocumented](https://man7.org/linux/man-pages/man1/head.1.html)
archaic use* of `head` (and `tail`) which apparently is only supported for back
compatibility.  It's equivalent to `-n 50`.  You can change `50` with any
number.  Some experimentation is needed with your terminal and the size of text
you have.

So there we have it.  A slightly convoluted way of getting something you
probably don't need, but seems to have always amused anyone I'm paring with
when I do it.

```bash
yes "" | head -50
```

*I'm hoping someone chimes in with a “why don't you just do X” so I can
un-muscle-memory this one.*
