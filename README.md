# freebird

What is freebird?
=================

freebird is a literate programming tool which aims to do as little as
possible. The user should write their documentation in their
documentation language of choice, and their code in their programming
language of choice, and freebird allows tangling or weaving to either
format without even needing to know anything about them.

It is heavily inspired by [Literate Haskell](https://wiki.haskell.org/Literate_programming),
which is [where the 'Bird-style' method of annotation originates.](https://www.haskell.org/onlinereport/literate.html).
However, it also borrows features from other literate programming paradigms ([like org-mode babel](https://orgmode.org/worg/org-contrib/babel/intro.html))

Vocabulary
==========

freebird borrows Knuth\'s vocabulary for the processing of literate
programming files:

- tangle: to produce source code from a literate programming document.
- weave: to produce documentation from a literate programming document.

For example, Knuth\'s original WEB system would take \'web\' files which
could be TANGLEd into compilable Pascal code, or WEAVEd (woven) into TeX
documentation.

How does it work?
=================

freebird mainly uses Bird-style to indicate code. This is, at its simplest, done like so:

```
> def hello(user):
>     print(f"Hello, {user}!")
```

i.e. code lines begin with \"\> \" (greater-than-sign, space). This is
in line with literate Haskell code, however there are some variants on it:

- \"\>\* \" (greater-than, asterisk, space) denotes code which should be hidden from woven documentation
  (e.g. import statements that don\'t add anything to your documentation) 
- \"\>\\ \" (greater-than, backslash, space) denotes a literal \'\>\', which is not taken as code for tangling, 
  and is woven as a \'\>\' (for example, for Markdown quote blocks)

As for knowing what to weave or tangle to, the freebird document should
start with a header, for example:

```
    FREEBIRD-->
    weave = ".md"
    tangle =  ".cpp"
    begin_src = "```cpp"
    end_src = "```" 
    <--
```

which is a TOML-formatted list of items: \"weave\": what file ending the
woven documentation should have. \"tangle\": what file ending the
tangled documentation should have. \"begin~src~\": how the weave
language begins a code block. \"end~src~\": how the weave language ends
a code block. Other optional header items can be added where supported.

In this example, the freebird document would be Markdown documentation
on C++ code.

Examples
========

Please see the Examples folder.

Syntax
======

The freebird syntax is as follows: FREEBIRD--\> indicates start of
freebird header. \<-- indicates end of freebird header.

Markers for code: 
- \> : marks a line of code. 
- \>\* : marks a \'hidden\' line of code (not shown in woven documentation)
- \>BEGIN : marks the start of a multi-line block of code. 
- \>END : marks the end of a multi-line block of code.

Goals
=====

- A lightweight literate programming format. Beyond the Bird-style syntax,
  all that\'s required is attaching a TOML document to the top of your
  page. 
- Extensibility. Further owing to the \'header\' format, it\'s easy
  for developers to add extra settings etc. for the processing of freebird
  documents. \'Plug-and-play\': freebird shouldn\'t ever need to know what
  you\'re using to write code or documentation, only what the export
  filetypes are called, and how to start and end a codeblock. 
- Complete portability: the freebird header system means that (in theory) nothing except
  freebird needs to be installed for freebird documents to be woven or tangled.

Non-goals
=========

- Invasive syntax. My problem with most literate programming systems is
  that I cannot directly write in my desired documentation language; I
  have to use the system's own syntax, and then spend ages figuring out
  how to make it interface with my language of choice. I feel the benefits
  here are on both sides:

  1.  the user already knows how to write both their code and
      documentation, so needs to learn minimal extra to start writing
      literate programs. They can also use whatever they like regardless
      of how arcane it is, rather than it having to be supported by the
      system.
  2.  backends don\'t need to be written for anyone\'s preference: I
      don\'t have to write an interface for TeX, then for Markdown, then
      HTML, roff, man pages...
