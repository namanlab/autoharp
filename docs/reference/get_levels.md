# Obtains the node levels from a tree.

This function obtains the node levels from a tree.

## Usage

``` r
get_levels(adj_list)
```

## Arguments

- adj_list:

  The adjacency list of the tree.

## Value

It returns a vector of integers. The length of this vector will be the
number of nodes in the tree. The root is at level 1, the next is at
level 2, and so on.

## Details

This function is used to check if the specification of the tree is in
BFS order. If that is indeed the case, the levels of each node should be
sorted.

This function is not exported for the general user.
