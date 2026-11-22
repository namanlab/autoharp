# Find the branch that leads from one node to another.

Given two nodes that are on the same path to the root, this function
determines the branch that leads to the child node.

## Usage

``` r
find_branch_num(th, child_id, ancestor_id)
```

## Arguments

- th:

  A TreeHarp object.

- child_id:

  An integer node id. It corresponds to the node to trace up from.

- ancestor_id:

  An integer node id. It corresponds to the node to trace down from.

## Value

An integer that denotes the branch to follow down (from the ancestor) to
reach the child.

## Details

This is used when trying to find a sub-call from a TreeHarp object. It
is useful in determining the indices to use when extracting the
sub-call.

## Examples

``` r
ex3 <- quote(x <- f(y = g(3, 4), z=1L))
t1 <- TreeHarp(ex3, TRUE)
find_branch_num(t1, 8, 3) # should be 1
#> [1] 1
find_branch_num(t1, 5, 3) # should be 2
#> [1] 2
```
