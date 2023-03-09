
# freebird

What is freebird?
=================

freebird is a [literate programming](http://www.literateprogramming.com/) tool which aims to do as little as
possible. The user should write their documentation in their
documentation language of choice, and their code in their programming
language of choice, and freebird allows tangling or weaving to either
format without even needing to know anything about them.

It is heavily inspired by [Literate Haskell](https://wiki.haskell.org/Literate_programming),
which is [where the 'Bird-style' method of annotation originates](https://www.haskell.org/onlinereport/literate.html).

Vocabulary
==========

freebird borrows [Knuth\'s vocabulary](https://en.wikipedia.org/wiki/Web_(programming_system)) 
for the processing of literate programming files:

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
>    print(f"Hello, {user}!")
```

i.e. code lines begin with "`> `" (greater-than-sign, space). This is
in line with literate Haskell code, however there are some variants on it:

- "`>* `" (greater-than, asterisk, space) denotes code which should be hidden from woven documentation
  (e.g. import statements that don\'t add anything to your documentation) 
- "`>\ `" (greater-than, backslash, space) denotes a literal \'\>\', which is not taken as code for tangling, 
  and is woven as the text '`> `' (for example, for Markdown quote blocks)

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

which is a TOML-formatted list of items:
- \"weave\": what file ending the woven documentation should have. 
- \"tangle\": what file ending the tangled source code should have. 
- \"begin_src\": how the weave language begins a code block. 
- \"end_src\": how the weave language ends a code block. 

Other optional header items can be added where supported.

In this example, the freebird document would be Markdown documentation
on C++ code.

Examples
========

Please see the `examples` folder.

Syntax
======

The freebird syntax is as follows: 
- `FREEBIRD-->` indicates start of freebird header. 
- `<--` indicates end of freebird header.

Markers for code: 
- `> `: marks a line of code. 
- `>* `: marks a \'hidden\' line of code (not shown in woven documentation)
- `>BEGIN`: marks the start of a multi-line block of code. 
- `>END`: marks the end of a multi-line block of code.

Benefits of freebird
====================

- A lightweight literate programming format. Beyond the Bird-style syntax,
  all that\'s required is attaching a TOML document to the top of your
  page. 
- Extensibility. Further owing to the \'header\' format, it\'s easy
  for developers to add extra settings etc. for the processing of freebird
  documents. 
- \'Plug-and-play\': freebird shouldn\'t ever need to know what
  you\'re using to write code or documentation, only what the export
  filetypes are called, and how to start and end a codeblock. This also 
  provides a shallow learning curve and more flexibility (through ignorance)
- Complete portability: the freebird header system means that (in theory) nothing except
  freebird needs to be installed for freebird documents to be woven or tangled.
- 

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
      system; it's just two languages interleaved, rather than both with a third system.
  2.  backends don\'t need to be written for anyone\'s preference: I
      don\'t have to write an interface for TeX, then for Markdown, then
      HTML, roff, man pages...

Recommendations for workflows
=============================

This section contains some recommendations for various programming workflows.

- A handful of issues which coincide:
  - My desired documentation format sucks to write in! (like HTML)
  - I want to publish my documents in multiple formats!

  The solution to all of these problems is [pandoc](https://pandoc.org/); write in whatever
  is comfortable for you, and then use pandoc to convert to other formats. For web publishing,
  either pandoc to HTML, or do the following:

  - I want to put my documentation on the web!

    As before, pandoc to HTML is an option. Otherwise, a better solution might be using
    something like [Sphinx](https://www.sphinx-doc.org/en/master/); through this you can
    write your documentation in .rst, weave it, and then build it with Sphinx.

Other options
=============

Here are some other literate programming systems.

- [NOWEB](https://www.cs.tufts.edu/~nr/noweb/) is probably the most popular;
  it supports any programming language, and has (La)TeX, HTML and troff backends. 
  Thus it is programming language independent, but *not* documentation language independent. It also has quite simple syntax.
- Some people use [Emacs Org-Mode with Babel](https://orgmode.org/worg/org-contrib/babel/index.html) for this purpose; 
  Emacs has an incredibly mature ecosystem and even simpler syntax 
  (code blocks just start and end with `#+BEGIN_SRC` and `#+END_SRC`) but it is again not documentation-independent, 
  and has a pretty steep learning curve. 
- [Jupyter Notebooks](https://jupyter.org/) are a newer option than the previous two.
  Being able to run code blocks independently and see their output is great
  (especially for quick feedback on code you're fiddling with) but it's not particularly well-built to work with anything
  except Python - it is also unsuitable for larger (than one file) projects or libraries, the files are not particularly
  readable without Jupyter (this is especially a problem for Git, partially remedied by [nbdime](https://github.com/jupyter/nbdime))
- [Literate Haskell](https://wiki.haskell.org/Literate_programming), as mentioned before, is freebird's main inspiration. 
  Of course, its biggest limitation is that it only supports Haskell code, as well as limited documentation
  languages. However, literate Haskell files can be directly compiled without needing to be tangled first.
