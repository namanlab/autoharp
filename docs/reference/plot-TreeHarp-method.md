# TreeHarp Plotting TreeHarp Objects

A plot method for visualising treeharp objects.

## Usage

``` r
# S4 method for class 'TreeHarp'
plot(x, y, ...)
```

## Arguments

- x:

  An object of class TreeHarp.

- y:

  Unused.

- ...:

  Additional arguments passed on to plot.igraph().

## Value

Returns NULL, invisibly.

## Details

The treeharp object is converted to an igraph object before it is
plotted.
