# Identifies if an apply function can improve the code

Identifies if an apply function can improve the code

## Usage

``` r
apply_can_improve_this(fname, window_size = 2)
```

## Arguments

- fname:

  A filename: either Rmd/qmd or R.

- window_size:

  A window size to analyse.

## Value

A numeric vector corresponding to windows for review is returned. If
none are found, then NULL is returned.

## Details

First, all library() calls and read() calls are removed. Next,
`only_actual_args_differ` is run on all pairwise expressions. The matrix
is searched for blocks of 1's along the diagonal.
