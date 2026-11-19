# Get the parent node id

This function retrieves the parent node id of a given node from an
adjacency list of a tree.

## Usage

``` r
get_parent_id2(adj_list, at_node)
```

## Arguments

- adj_list:

  The adjacency list of the tree.

- at_node:

  The node whose parent should be extracted.

## Value

A integer of length 1 should be returned for all nodes except the root.
For the latter, the function will return NULL.

## Details

Remember that the list has to be for a tree, not a general graph. Please
see other help pages for more specifications.

This is a low-level function, used within the S4 class TreeHarp. It is
not generally meant for use by the user.

If there are nodes that have more than one parent, then a warning is
issued.
