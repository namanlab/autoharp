# Obtain an index to extract out a sub-call

Obtains an index that can be used to extract a sub-call from a language
object.

## Usage

``` r
get_recursive_index(th, node_id)
```

## Arguments

- th:

  A TreeHarp object.

- node_id:

  An integer corresponding to a call within the parse tree (not a
  literal, symbol or a formal argument).

## Value

A vector of indices, that can be used (together with "\[\[") to obtain a
sub-call

## Examples

``` r
ex3 <- quote(x <- f(y = g(3, 4), z=1L))
t1 <- TreeHarp(ex3, TRUE)
rec_index <- get_recursive_index(t1, 6)
ex3[[rec_index]]
#> g(3, 4)
ex3[[get_recursive_index(t1, 3)]]
#> f(y = g(3, 4), z = 1L)
```
