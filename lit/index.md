---
title: Tangling Code
---

We have to extract source code from a "literate Markdown" file, then
place it somewhere.

Following Knuth, we call these snippets of code (these Oases in a
desert of prose) "Chunks" and they are described in the `chunk.sml`
file. 

A chunk consists of some metadata (described in the `metadata.sml`
file) and a substring of the "code proper". When extracting code from
a literate program, it's the "code proper" we're writing somewhere.

This requires parsing a Markdown file for its code snippets, collating
the chunks according to file, and then handing this off to someone
else.

The main program which determines where to extract code, which files
to pull it from, etc., is the `tangle.sml` file.

The contents of this program consists of:

- [`metadata.sig` and `metadata.sml`](./metadata.md)
- [`chunk.sig` and `chunk.sml`](./chunk.md)
- [`parser.sig` and `parser.sml`](./parser.md)
- [`tangle.sig` and `tangle.sml`](./tangle.md) and MLton's preferred
  Poly/ML-compatible `main.sml` file

<footer>

**[** [Parent](../index.md) **]**

</footer>
