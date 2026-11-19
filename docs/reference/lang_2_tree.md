# Convert language object to tree.

A recursive function for converting a language object to treeharp.

## Usage

``` r
lang_2_tree(lang_obj, node_id, ni_env)
```

## Arguments

- lang_obj:

  A language object.

- node_id:

  The calling node to this language object. This should only be greater
  than 0 if the `ni_env` already contains a partial adjacency list and
  corresponding node information. This will happen when this function is
  called recursively.

- ni_env:

  An environment to store the adjacency list and node information.

## Value

Nothing

## Details

This function is used by TreeHarp constructors. It should not have to be
called by a user. It works by bulding up an adjacency list and node node
information data frame within the supplied environment.

## Examples

``` r
e1 <- new.env() 
lang_2_tree(quote(X <- 1), 0, e1) 
e1$adj_list 
#> $`<-`
#> [1] 2 3
#> 
#> $X
#> NULL
#> 
#> $`1`
#> NULL
#> 
e1$node_info
#>   id name call_status formal_arg depth
#> 1  1   <-        TRUE      FALSE     1
#> 2  2    X       FALSE      FALSE     2
#> 3  3    1       FALSE      FALSE     2
```
