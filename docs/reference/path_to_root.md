# Extract a path from node to root.

Identifies the nodes on the path from a node up to the root of a
TreeHarp object.

## Usage

``` r
path_to_root(th, node_num)
```

## Arguments

- th:

  A TreeHarp object.

- node_num:

  A node number to start tracking upwards from.

## Value

A vector of 1's and 0's that can be used to carve out the branch alone,
using
[`carve_subtree`](https://github.com/namanlab/autoharp/reference/carve_subtree.md).

## Details

This function allows the user to identify the branch from a node up to
the root of a tree.

## Examples

``` r
ex1 <- quote(x <- f(y, g(5)))
th1 <- TreeHarp(ex1, TRUE)
path_to_root(th1, 5)
#> [1] 1 0 1 0 1 0
```
