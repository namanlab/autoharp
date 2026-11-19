# Convert to TreeHarp objects

Reads in an Rmd file or an R script and converts it to a list of
TreeHarp objects.

## Usage

``` r
rmd_to_forestharp(fname, verbose = FALSE)
```

## Arguments

- fname:

  The filename that is to be read in.

- verbose:

  If TRUE, then messages will be printed to enable debugging. dropped.

## Value

There are three possible return outcomes: (1) NA, indicating that
[`purl`](https://rdrr.io/pkg/knitr/man/knit.html) was unable to extract
the R expressions from the Rmd/qmd file. (2) A list with two components,
named `forest` and `line_nums`, each being NA. (3) A list with two
components, named as in (2), containing the TreeHarp objects and a
vector of line numbers.

If there are syntax errors in the file, the expressions may not be
parsed correctly. The `verbose` option will reflect this.

## Details

If an Rmd or qmd file is supplied as input, it is first converted to an
R script before
[`get_source_expressions`](https://lintr.r-lib.org/reference/get_source_expressions.html)
is used to try to parse the expressions. These expressions are then
matched to the original file to retrieve line numbers. Unlike previous
versions of this function, line numbers will always be returned.

Expressions that could not be parsed will be returned as NA.

Line numbers are extracted using
[`get_source_expressions`](https://lintr.r-lib.org/reference/get_source_expressions.html)
from the lintr package.

## See also

[`fapply`](https://github.com/namanlab/autoharp/reference/fapply.md),
[`extract_chunks`](https://github.com/namanlab/autoharp/reference/extract_chunks.md),
[`extract_chunks`](https://github.com/namanlab/autoharp/reference/extract_chunks.md),
[`get_source_expressions`](https://lintr.r-lib.org/reference/get_source_expressions.html)
