# Reset search path of current R session

This function is used to detach packages that have been added by a
student script.

## Usage

``` r
reset_path(old_path)
```

## Arguments

- old_path:

  A character vector of package namespaces. This is usually the output
  of [`search`](https://rdrr.io/r/base/search.html), run before an R
  script or Rmd file is rendered, which could cause the search path to
  change.

## Value

There is no object returned. This function is called for it's side-
effect of altering the search path.

## Details

When a student script is rendered using
[`render_one`](https://github.com/namanlab/autoharp/reference/render_one.md),
new packages might be added to the search path. These may conflict with
the instructors' search path order, or with subsequent runs of
[`render_one`](https://github.com/namanlab/autoharp/reference/render_one.md)
on students. Hence there is a need to reset the search path before this
is done.

This function does not unload namespaces. It only detaches them from the
search path. For a difference between the two, please see Hadley's page.

## Examples

``` r
opath <- search()
# Load a package
reset_path(opath)
```
