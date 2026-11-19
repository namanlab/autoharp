# Copy an object from one env to another.

A wrapper function that uses assign and get.

## Usage

``` r
copy_e2e(from_obj, from_env, to_obj, to_env)
```

## Arguments

- from_obj:

  The name of the object to copy. It has to be a string.

- from_env:

  The environment in which the object lives. It has to be an object of
  class environment.

- to_obj:

  The name of the object to assign it to, in the new environment. Also a
  string.

- to_env:

  The environment to which the new object is to be assigned. It has to
  be an object of class environment.

## Value

There is no return value. This function is called for its' side effect.

## Examples

``` r

e1 <- new.env(); e2 <- new.env()
ls(e2)
#> character(0)
evalq(x <- 1L, e1)
copy_e2e("x", e1, "y", e2)
ls(e2)
#> [1] "y"
```
