## Markdown as a String Interpolator

Proof of concept of a markdown string interpolator. E.g. you will be able to write

``` scala
val mads = "Mads"

md"""
# An example

This is an example of $mads
"""
```

and produce a value in a number of output formats (`String`, scalajs-react, etc.)

Like MDX but not tied to Javascript and React.
