+++
title = "Language Servers Are Cool"
date = 2020-03-01T14:56:43Z
images = []
tags = []
categories = []
draft = true
+++

In slight contrast to my previous post where I espoused the
virtue of code being text and not  relying on developer tooling
to interact with it: language servers are cool.

They decouple the back end of gaining semantic understanding
of code in a particular language from the editor implementation.
A [language server for each language][langserverlist] can be
written, and the editor only needs to integrate the [language
server protocol][lsp] once, to get a host of IDE style features
for each language.

This is the sort of solution we need to get rid of editor lock
in, and allow text editors to compete to be the best at editing
text.  With language servers, adding language support for an
editor is cheap. Hopefully now developers will start choosing
text editors on the grounds of how good they are at editing text,
rather than the shopping list of languages they support.

I've used [rls][rls] for rust, and [clangd][cd] for C++ with the
[languageClient-neovim][lcnv] for neovim. I've been the envy of
colleagues who use Vim-like editors for the impressive language
support, and the envy of colleagues who use IDEs for the superior
text editing... Not really.  They don't see why I can't just
install their IDE and be done with it. Anyway,language servers
are a great bit of technology.

[langserverlist]: https://langserver.org/
[lsp]: https://microsoft.github.io/language-server-protocol/
[rls]: https://rls.booyaa.wtf/#neovim
[cd]: https://github.com/autozimu/LanguageClient-neovim/wiki/Clangd
[lcnv]: https://github.com/autozimu/LanguageClient-neovim/
