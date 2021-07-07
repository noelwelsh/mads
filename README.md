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

It's like MDX but not tied to Javascript and React.

To use, add the following dependency to your `build.sbt`:

``` scala
libraryDependencies += "org.creativescala" %% "mads" % "0.1.0"
```

### Supported Markdown

Mads does not currently support much of markdown. The following is implemented:

- Headings in the sane ATX format (`# Heading`)
- Paragraphs

More will be added as the need arises. It's unlikely it will ever support Setext
headings.


### How it Works

It uses a so-cool-it's-frozen parser combinator library that allows suspending
and resuming parsing.


### Publishing

To publish a release:

- update the version number on master in publish.sbt
- update the version number in README.md
- run + publishSigned
- run sonatypeBundleRelease
- tag master with the release version
- push tags to origin.
