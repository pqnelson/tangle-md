Tangle-MD (or, Doctor Tangles) extracts source code from a literate
Markdown "web" file.

Right now, this is a very preliminary version which just looks for all
code fences with a language, then extracts their contents.

If you want to avoid inclusion, you could add an asterisk to the
language name, e.g.,

    ```sml*
    (* example code which won't be extracted by Doctor Tangles *)
    ```
    
Right now, it blindly takes **everything**, including code fences
describing different languages. Obviously this is not ready for "prime time",
but we're getting there...

# License

This is released under the MIT License, which should be available in the
[LICENSE](./LICENSE) file.

# Usage

There are a few supported metadata fields supported:

- `example` will toggle a codefenced block as an "example", i.e., not
  included for export as sourcecode --- this is the same as adding a
  `*` suffix to the language name.
- `file = filename` will export the current codefenced environment
  (and all future codefenced environments) to the file
  "filename". When another codefenced block is encountered with
  another `file = different_filename`, the output will be redirected.
  If later a codefenced environment has `file = filename`, then the
  code chunks are **appended** to the code chunks which will be
  directed towards file "filename".
  
A codefence block has its first line be `` ```language
{metadata-field-1=value-1, ...}``. Most Markdown engines will
disregard this, or (at most) use it for syntax highlighting.

# About

This is part of a larger ecosystem of tools. For example, I intend to
use it in conjunction with [`md`](https://github.com/pqnelson/md/).

The unit tests require the
[`sml-xunit`](https://github.com/pqnelson/sml-xunit/) library.
You may have to update the [`tests.mlb`](./tests.mlb) file to point to
the correct location of `sml-xunit/xunit.mlb`.
