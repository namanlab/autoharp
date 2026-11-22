# Generate the next sub-tree.

This generates the next sub-tree in the enumeration list.

## Usage

``` r
get_next_subtree(obj, char_arr)
```

## Arguments

- obj:

  An object of class TreeHarp.

- char_arr:

  A vector of 1's and 0's indicating which nodes to keep. The vector
  should have length equal to the number of nodes in obj.

## Value

A vector of 1's and 0's, which denotes the next sub-tree in the list.

## Details

Need to reference the paper. This generates the next sub-tree, rooted at
the root node of this tree. It will generate singletons on it's own. It
has to be used within a loop to do that.

## See also

[`generate_all_subtrees`](https://github.com/namanlab/autoharp/reference/generate_all_subtrees.md)

## Examples

``` r
th1 <- TreeHarp(list(a=c(2,3), b=NULL, c=NULL))
get_next_subtree(th1, c(1,0,0))
#> [1] 1 0 1
get_next_subtree(th1, c(1,1,0))
#> [1] 1 1 1
```
