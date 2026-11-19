# Extract chunks that match a pattern.

Extracts chunks whose labels match a pattern from the rmd file.

## Usage

``` r
extract_chunks(rmd_name, pattern)
```

## Arguments

- rmd_name:

  A character string, the name of the rmd file to get the chunks from.

- pattern:

  The pattern to match within the label. In fact, the match is applied
  to the whole chunk option.

## Value

A list of character vectors. Each vector contains the chunk from the
file. If no pattern is specified, all chunks are returned. Remember that
the chunk header and tail are also included in the returned list.
