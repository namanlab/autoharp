# Checks if a graph is connected.

A tree is a graph that is connected but does not have any cycles. This
function checks if a provided adjacency list is connected.

## Usage

``` r
is_connected(adj_list, root = 1)
```

## Arguments

- adj_list:

  The adjacency list of the tree.

- root:

  The root node to start checking from. This defaults to the first node
  in the adjacency list.

## Value

The function returns a TRUE if the graph is connected and FALSE
otherwise.

## Details

This function is used as one of the validity checks within the
definition of the TreeHarp class. It is a low-level function, not really
meant for the general user of the package. Hence it is not exported.

The nodes are traversed in a BFS order. The function could actually be
combined with
[is_cyclic_r](https://github.com/namanlab/autoharp/reference/is_cyclic_r.md),
but it is kept separate for modularity reasons.

An alternative was to convert the list to an adjacency matrix and check
for a row and column of zeros.
