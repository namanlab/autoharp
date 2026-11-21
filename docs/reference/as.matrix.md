# TreeHarp Cast a TreeHarp to Matrix.

Convert a treeharp object to an adjacency matrix.

## Usage

``` r
# S4 method for class 'TreeHarp,matrix'
coerce(from, to = "matrix", strict = TRUE)
```

## Arguments

- from:

  A treeharp object.

- to:

  The class to convert to; always `"matrix"` here.

- strict:

  Not used; it comes from the `coerce()` generic.

## Value

A matrix.
