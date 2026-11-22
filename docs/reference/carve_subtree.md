# Carve out branches to form a new tree.

This functions keeps only the indicated nodes, returning a new sub-tree.

## Usage

``` r
carve_subtree(obj, char_arr)
```

## Arguments

- obj:

  An object of class TreeHarp.

- char_arr:

  A vector of 1's and 0's indicating which nodes to keep. The vector
  should have length equal to the number of nodes in obj.

## Value

An object of class TreeHarp.

## Details

This returns an error if the sub-tree does not define a new tree.

## Examples

``` r
th3 <- list(a= c(2L,3L,4L), b=NULL, c=c(5L, 6L), d=7L, e=NULL, f=NULL, g=NULL)
carve_subtree(TreeHarp(th3), c(1,0,0,0,0,0,0))
#> a 
st <- subtree_at(TreeHarp(th3), 4)
plot(st)
```
