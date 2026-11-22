# Extract a sub-tree.

Extracts a sub-tree rooted at a particular node.

## Usage

``` r
subtree_at(obj, at_node, preserve_call = FALSE)
```

## Arguments

- obj:

  An object of class TreeHarp

- at_node:

  The root of the new sub-tree. An integer, not a label, that
  corresponds to BFS indexing of the tree.

- preserve_call:

  A logical value that indicates if a sub-call should be extracted. This
  might be slower, but it allows you to evaluate it later.

## Value

An object of class TreeHarp.

## Details

This is meant for internal use, so the nodeTypes slot is silently
dropped, unless preserve_call is set to TRUE

## Examples

``` r
th3 <- list(a= c(2L,3L,4L), b=NULL, c=c(5L, 6L), d=7L, e=NULL, f=NULL, g=NULL)
subtree_at(TreeHarp(th3), 3)
#> c-e-f 
st <- subtree_at(TreeHarp(th3), 4)
plot(st)
```
