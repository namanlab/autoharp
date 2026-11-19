# Get the children node ids

This function retrieves the child node ids of a given node from an
adjacency list of a tree.

## Usage

``` r
get_child_ids2(adj_list, at_node)
```

## Arguments

- adj_list:

  The adjacency list of the tree.

- at_node:

  The node whose children should be extracted.

## Value

A vector of integers specifying the children of that particular node. If
the node is a leaf, it returns NULL.

## Details

Remember that the list has to be for a tree, not a general graph. Please
see other help pages for more specifications.

This is a low-level function, used within the S4 class TreeHarp. It is
not generally meant for use by the user.
