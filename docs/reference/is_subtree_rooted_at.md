# Checks if a tree is rooted at a node of another tree.

This function checks if a given tree is a sub-tree of another tree at a
particular node.

## Usage

``` r
is_subtree_rooted_at(x, y, at_node)
```

## Arguments

- x:

  An object of class TreeHarp.

- y:

  An object of class TreeHarp.

- at_node:

  An integer, corresponding to a node in object y. The sub-tree of y,
  rooted at at_node, is compared to x.

## Value

A logical value indicating if x is a sub-tree of y, rooted at at_node.

## Details

Here's how it works: The sub-tree of y, rooted at at_node is first
extracted. The tree x is then compared to this. If x is a sub-tree of
it, then this function returns FALSE. Otherwise it returns TRUE.

## Examples

``` r
thb1 <- TreeHarp(list(b=2, d=NULL))
tha1 <- TreeHarp(list(a=c(2,3), b=4, c = NULL, d=NULL))
is_subtree_rooted_at(thb1, tha1, 1) # FALSE
#> [1] FALSE
is_subtree_rooted_at(thb1, tha1, 2) # TRUE
#> [1] TRUE
```
