# Count tokens in R/Rmd

Count the individual tokens. Part of the NLP analysis process.

## Usage

``` r
rmd_to_token_count(fname, include_actuals = TRUE)
```

## Arguments

- fname:

  The Rmd or R file name.

- include_actuals:

  Whether actual arguments/literals should be included. If this is
  FALSE, then only calls and formal arguments will be used in the count.

## Value

A tibble. The tibble will contain a the frequency count for all tokens
present in the student script.
