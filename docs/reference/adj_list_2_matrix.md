# Convert adjacency list to a matrix

Converts a list that represents a tree into a binary matrix.

## Usage

``` r
adj_list_2_matrix(adj_list)
```

## Arguments

- adj_list:

  The adjacency list of the tree.

## Value

A symmetric matrix of 1's and 0's, with 1 in entry (i,j) representing an
edge between the two vertices.

## Details

Remember that the list has to be for a tree, not a general graph. Please
see other help pages for more specifications.

This is a low-level function, used within the S4 class TreeHarp. It is
not generally meant for use by the user.

It works by filling up the upper diagonal of the matrix before
reflecting it.
