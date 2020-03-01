+++
title = "You Shouldn't Need an IDE to Read Code"
date = 2020-02-29T22:30:09Z
images = []
tags = []
categories = []
draft = true
+++

This is important.

Code is _read_ far more often than it's _written_.

Primarily, it's read through a web GUI.

You want to read the source code of a library you're using.  Say it's in
Github, you're going to start looking at it through the Github GUI. If you want
to take a deeper look, you might clone the repo and open it in you're editor,
but the vast majority of library code you read will be through a GUI.

The default medium for reading the changes in a pull request is through a GUI.
Sure, you can pull the branch and open the code up in your editor. When I'm
reviewing big changes I do just that. But I shouldn't be required to.

I've reviewed code before where I've made a comment along the line of "this
line of code doesn't explain what it's doing", where I've suggested type
hinting, or renaming things to reflect what's happening, or refactoring the
aggregations that data flows between each line of code so it would make more
sense in the context of the function.  Frustratingly, my comments have received
responses that essentially say: "if you were using my editor, it makes perfect
sense".

My recent concrete example is reviewing code in rust. Rust lets you optionally
type hint in various places, e.g. to specify variable `x` as type
`SpecificType`:

```rust
let x: SpecificType = get_a_value();
```

There are times where usually optional type declarations are necessary for
compilation because the compiler can't infer the types in your code, but for
the most part, these type hints are optional and can be used by the writer of
the code to check their work or by the reader of the code to understand what's
happening at a type level.

In rust, types matter. When you're writing and reviewing rust code, you're
thinking about types.  I recently suggested adding some type hinting to some
rust code to make it more readable. I was told, in response, that the type
hinting was unnecessary, and if I was only using the same editor[^1] I could
hover my mouse over the line of code and see the information I need.

I take issue with both of these statements:

* Yes, the code will compile without the type hinting, but code is not there to
  satiate compilers, it's there to communicate ideas between people. And,
* What if I don't? Am I required a specific editor with a specific setup in
  order to read your code? A community of developers being locked into specific
  tooling is not good.  Even if I was using the same editor setup that gives me
  all the IDE features, the web GUI I'm likely to review code through doesn't.

Editor lock-in is a problem.  I've not developed in Java, but I've been told
it's very difficult to use without an IDE setup for it.

Everyone has their dev environment setup in a way that suits them. Code is
there to communicate ideas between developers. Requiring someone installing a
bunch of tooling to be able to read your code is mad.

Maybe I'm biased. I use Vim (neovim). I use some IDE like plugins, but mostly
treat code as text, because that's what it is. And Vim is the best text editor
I've ever used. Setting up the developer features that IDEs give you out of the
box is possible, but takes time to set up and tune.  So I tend not to have much
in the way of these features, especially for languages that I don't write in
every day.  So maybe I'm just trying to avoid the self imposed faff of setting
up my editor so I can read code in the same way someone else does.

But I don't think so. We don't predominantly read code through editors. We want
to avoid entangling simple tasks like reading code with a load of extra
requirements.  These developer tools are useful, but we shouldn't become
reliant on them.  Code is text.  It should only require a vanilla text editor
to write, and only require something that can render text to read.

[^1]: VSCode appears to be the senior editor for rust.  I don't have a problem
with this.  By the looks of it, it's a fine GUI based editor and everyone
I know who uses it loves it.  It seems its struck a great balance between
having great functionality out of the box, while being easily tuneable and
feel sharp. If I hadn't drank the Vim coolaid, I imagine I'd be using it.
