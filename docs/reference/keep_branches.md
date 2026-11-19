# Keep only branches specified by node numbers

Retains only specific branches, that are identified by their node
numbers.

## Usage

``` r
keep_branches(th, branch_nodes, include_lower = TRUE)
```

## Arguments

- th:

  A TreeHarp object.

- branch_nodes:

  An integer vector, specifying the nodes to keep.

- include_lower:

  A logical value - whether or not the lower branches should also be
  kept.

## Value

A TreeHarp object.

## Examples

``` r
ex1 <- quote(x <- f(y, g(5)))
th1 <- TreeHarp(ex1, TRUE)
keep_branches(th1, 3)
#>  
keep_branches(th1, 3, include_lower = FALSE)
#>  
keep_branches(th1, c(2,3), FALSE)
#>  
keep_branches(th1, c(3, 4), FALSE)
#>  
```
