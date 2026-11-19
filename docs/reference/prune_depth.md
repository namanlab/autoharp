# Prune a tree up to a specified depth.

Prunes a tree up to a depth specified by a set of node names.

## Usage

``` r
prune_depth(th, names_to_keep)
```

## Arguments

- th:

  A TreeHarp object.

- names_to_keep:

  The node names to keep in the pruned tree.

## Value

An object of class TreeHarp.

## Details

This is a seldom used function. It works in this way. Given a set of
node names, it identifies the node with the greatest depth in that set.
The function then returns the sub-tree, that contains all the nodes with
a depth smaller than or equal to that depth. If the node types slot is
not NA, then that data frame is filtered and returned too.

Take a look at the examples for a clearer picture.

## See also

[`carve_subtree`](https://github.com/namanlab/autoharp/reference/carve_subtree.md),
[`path_to_root`](https://github.com/namanlab/autoharp/reference/path_to_root.md),
[`carve_mst`](https://github.com/namanlab/autoharp/reference/carve_mst.md)

## Examples

``` r
ex1 <- quote(x <- f(y, g(5)))
th1 <- TreeHarp(ex1, TRUE)
s1 <- prune_depth(th1, c("f", "y"))
s2 <- prune_depth(th1, c("f", "z")) # node not present!
plot(s1)

plot(s2)
```
