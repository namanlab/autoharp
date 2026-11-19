# Calculates the Total Memory Used

This function uses the utils package to compute the total amount of
memory used by objects in an environment.

## Usage

``` r
env_size(env)
```

## Arguments

- env:

  The environment whose size is to be computed.

## Value

The size in bytes, as a numeric value (scalar).

## Details

The names are wrapped in backticks. Otherwise, non-syntactic names will
cause problems. This function is used within
[`render_one`](https://github.com/namanlab/autoharp/reference/render_one.md)
as part of the runtime stats assessment.

## Examples

``` r

e1 <- new.env()
env_size(e1)
#> [1] 0
evalq(x <- 1:10000L, e1)
env_size(e1)
#> [1] 40048
```
