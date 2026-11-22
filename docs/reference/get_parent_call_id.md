# Get Node Id of Parent Call

Get the node id of the parent call for a given node.

## Usage

``` r
get_parent_call_id(x, node_id)
```

## Arguments

- x:

  A TreeHarp object.

- node_id:

  The id of the node whose parent call is to be found. An integer value.

## Value

An integer corresponding to the node id of the calling function.

## Details

When we need to go up the parse tree to obtain the function that called
this node, we use this function. It is similar to `get_parent_id`,
except that that function only returns the immediate parent.

It is not useful to call this function when the TreeHarp object is not
constructed from a language object.

Perhaps this function is necessary only because of the way language
objects are represented by the autoharp: formal arguments are included
in the tree representation. When we wish to find the calling function,
we have to walk up the branches till we reach a function call.

## See also

[`get_parent_id`](https://github.com/namanlab/autoharp/reference/get_parent_id.md)

## Examples

``` r
ex3 <- quote(x <- f(y = g(3, 4), z=1L))
t1 <- TreeHarp(ex3, TRUE)

# get the function that calls g:
get_parent_call_id(t1, 6) 
#> [1] 3
#contrast with this:
get_parent_id(t1, 6)
#> [1] 4
```
