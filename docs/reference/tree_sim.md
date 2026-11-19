# Compute tree similarity

Computes similarity between two trees (non-recursively)

## Usage

``` r
tree_sim(t1, t2, norm = FALSE, ...)
```

## Arguments

- t1:

  A TreeHarp object

- t2:

  Anothe TreeHarp object.

- norm:

  A logical value, indicating if the kernel function should be
  normalised, to account for different tree lengths.

- ...:

  Unused arguments, reserved for mcmapply

## Value

A numerical value between 0 and 1 (if normed).
