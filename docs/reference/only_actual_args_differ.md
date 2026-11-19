# Checks if two expressions differ only in terms of actual arguments

Checks if two expressions differ only in terms of actual arguments

## Usage

``` r
only_actual_args_differ(th1, th2)
```

## Arguments

- th1:

  A TreeHarp object.

- th2:

  A TreeHarp object.

## Value

Either 1 or 0. 1 means that the two expressions differ only in their
actual arguments.

## Details

From the node types, only calls and formal arguments are retained. If
these are identical, then a 1 is returned. Otherwise 0 is returned.

## Examples

``` r
ex1 <- TreeHarp(quote(X <- rnorm(10, mean=0.9, sd=4)), TRUE)
ex2 <- TreeHarp(quote(Y <- rnorm(20, mean=9, sd=4)), TRUE)
only_actual_args_differ(ex1, ex2)
#> [1] 1
```
