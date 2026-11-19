# Identify if use of pipe operator can improve a code section

Identify if use of pipe operator can improve a code section

## Usage

``` r
pipe_can_improve_this(fname, window_len = 2)
```

## Arguments

- fname:

  A filename - either a Rmd/qmd or R script.

- window_len:

  A window length to analyse.

## Value

A numeric vector corresponding to start of the window to be reviewed. If
no lines are found, NULL is returned.

## Details

A rolling window approach is used here. For each expression, the
assigned object is extracted (if any). Subsequent lines are checked to
see if this object appears as an actual argument.

Window length two means that only one more line is checked.
