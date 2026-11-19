# Carve a Minimal Spanning Tree Out

Given node names, this function retrieves the smallest tree containing
at most those nodes.

## Usage

``` r
carve_mst(th, node_names)
```

## Arguments

- th:

  An object of class TreeHarp.

- node_names:

  A character vector of node names. Nodes outside this set will not be
  returned in the tree. It must include the root node name.

## Value

An object of class TreeHarp.

## Details

The function starts from each node specified and works it's way up to
the root. If a branch contains nodes outside the list, it is shortened.

In the end, the tree that is returned will try to contain all the named
nodes, but if that's not possible some will dropped to ensure a tree is
returned, not a disconnected graph.

## Examples

``` r
ex1 <- quote(x <- f(y, g(5)))
th1 <- TreeHarp(ex1, TRUE)
carve_mst(th1, c("<-", "x", "f", "5")) ## note: 5 is dropped.
#>  
carve_mst(th1, c("<-", "x", "f", "y")) 
#>  
carve_mst(th1, c("<-", "f", "g")) 
#>  
```
