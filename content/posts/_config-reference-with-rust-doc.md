+++
title = "Using Rust Doc to Generate a Config File Reference"
date = 2020-07-15T14:58:19+01:00
images = []
tags = []
categories = []
draft = true
+++

There's something missing from the documentation of CLI tools.

They often have home pages that do a great job of explaining the core concepts,
giving a quickstart guide, and demonstrating some use cases.

Where I find tools' documentation is missing something, is when you're already
well acquainted with the tool, and you want to know some specific detail about
a single field like what possible values it has or exactly where it sits in the
structure of the config. I find myself really in need of a "config file
reference" akin to the ones you see for APIs which get generated from API Specs
or code.

Having worked with the Kubernetes API a fair bit over the last few years I find
myself going straight to the API reference whenever I need to look something
up, as I'm normally looking up the specifics of where a field lives or what it
does. Having documentation that allows you to follow links to navigate through
the structure of the API objects is really helpful

It's often really hard to find out the specifics of a config field if it
doesn't appear in examples or some explanation. Projects that make an attempt
to provide a reference for config seem to follow the same pattern.  For
relatively simple config files they provide an example with all the fields,
optional and not, provided, and then comment the example to say what's option,
what the possible values are.

TODO: example picture

This works OK while the config is small and simple.  But as config grows in
size and becomes more complex in terms of what fields are needed, where
sections of config could have alternatives, or be required only if some other
config is set, it starts to get out of hand.

It seems beyond a certain point projects, that do provide a config reference,
resort to a more protracted form of a web page that lists each field and
provides the information on it.  This produces something that is complete, but
isn't always that easy to navigate, scrolling is often your only resort, and I
often find I lose track of where a field is nested within the structure.

In both approaches mentioned there, there is the problem that the config file
reference is maintained separately from the code that defines it.  For
maintainability and correctness' sake, we really want the code that defines
config, and the documentation that attempts to provide a complete and correct
reference for it, to be together.

I've been developing a CLI tool at work in recent months.  It's written in
Rust.  I noticed the Rust docs going largely unused as the crate is a binary.

I've recently experimented with using using Rust docs to generate a reference
for a config file from the data structure that defines defines the schema for
it.

_TL;DR: This kinda works. The generation from code and and the structure to the
document is great, but there's too much "other things" beyond the structure of
the config in view.  While this is parsible for someone familiar with Rust
documentation, wider readers could get quite confused. If the Rust
representation of the data stops being simple field names without many
annotations that rename or flatten the data it becomes harder to communicate
the exact structure of the config in the config file._
