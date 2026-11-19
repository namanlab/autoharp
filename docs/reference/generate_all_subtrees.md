# Generate all subtrees from a tree.

This routines generates all subtrees rooted at the root node for a
particular tree.

## Usage

``` r
generate_all_subtrees(th)
```

## Arguments

- th:

  An object of class TreeHarp.

## Value

A 0-1 matrix with n rows and m columns. n is the number of sub-trees
rooted at the root node of th. m is the number of nodes in this given
tree. The leading column will be a 1 for all the rows.

## References

*Listing and counting subtrees of a tree*, F Ruskey, *SIAM Journal on
Computing*, 1981

## See also

[`get_next_subtree`](https://github.com/namanlab/autoharp/reference/get_next_subtree.md)

## Examples

``` r
th1 <- TreeHarp(list(a=c(2,3), b=NULL, c=NULL))
generate_all_subtrees(th1)
#>      [,1] [,2] [,3]
#> [1,]    1    0    0
#> [2,]    1    0    1
#> [3,]    1    1    0
#> [4,]    1    1    1
```
