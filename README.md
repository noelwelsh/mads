## Markdown as a String Interpolator

Proof of concept of a markdown string interpolator. E.g. you can write

``` scala
import mads.Mads.text._

val mads = "Mads"

md"""
# An example

This is an example of $mads
"""
```

and produce a value in a number of output formats (`String`, scalajs-react,
etc.) By "a number of output formats" I mean text, which is currently the only
supported format---but other formats could easily be added.

Like MDX but not tied to Javascript and React.


### Supported Markdown

Mads does not currently support much of markdown. The following is implemented:

- Headings in the sane ATX format (`# Heading`)
- Paragraphs

More will be added as the need arises. It's unlikely it will ever support Setext
headings.


### How it Works

It uses a so-cool-it's-frozen parser combinator library that allows suspending
and resuming parsing.
