# Computes Jaccard Index

Computes the Jaccard index between two trees.

## Usage

``` r
jaccard_treeharp(th1, th2, weighted = FALSE)
```

## Arguments

- th1:

  A TreeHarp object.

- th2:

  A TreeHarp object.

- weighted:

  A logical value, indicating if the weighted Jaccard similarity should
  be computed.

## Value

A real number between 0 and 1.

## Details

The unweighted form is just the cardinality of the intersection of the
two sets of tokens, divided by the union of the two sets.

The weighted form is described on the WIkipedia page:
https://en.wikipedia.org/wiki/Jaccard_index#Weighted_Jaccard_similarity_and_distance
