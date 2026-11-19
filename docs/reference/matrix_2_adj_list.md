# Convert adjacency matrix to a list.

Converts a binary matrix that represents a tree into an adjacency list.

## Usage

``` r
matrix_2_adj_list(mat)
```

## Arguments

- mat:

  A symmetric matrix of 1's and 0's, with 1 in entry (i,j) representing
  an edge between the two vertices.

## Value

The adjacency list of the tree.

## Details

Remember that the list that is finally output is for a tree, not a
general graph. Please see other help pages for more specifications.

The input matrix should be BFS ordered. The adjacency list only notes
the child node(s) of a particular node. If a matrix denotes multiple
parents, it will not be picked up.

This is a low-level function, used within the S4 class TreeHarp. It is
not generally meant for use by the user.
