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

# About

This is part of a larger ecosystem of tools. For example, I intend to
use it in conjunction with [`md`](https://github.com/pqnelson/md/).

The unit tests require the
[`sml-xunit`](https://github.com/pqnelson/sml-xunit/) library.
You may have to update the [`tests.mlb`](./tests.mlb) file to point to
the correct location of `sml-xunit/xunit.mlb`.