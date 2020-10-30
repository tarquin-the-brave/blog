+++
title = "Using Rust Doc to Generate a Config File Reference"
date = 2020-07-15T14:58:19+01:00
images = []
tags = []
categories = []
draft = true
+++

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
