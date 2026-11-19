# Conducts checks before rendering file

This routine conducts two checks before rendering the file: presence of
View function, and presence of system() calls. The former is a nuisance,
while the latter is possibly nefarious.

## Usage

``` r
render_prechecks(fname, in_place = TRUE, new_name, verbose = FALSE)
```

## Arguments

- fname:

  The name of the Rmd/qmd file to check.

- in_place:

  If TRUE, the original file will be overwritten in place.

- new_name:

  If in_place is FALSE, this should be the full path to the new file to
  be written (including directory location).

- verbose:

  If TRUE, debugging messages will be printed.

## Value

Either TRUE or FALSE. If TRUE, all View() lines have been commented, and
there are no system calls. If FALSE, it means there are system calls (to
be screened further).
