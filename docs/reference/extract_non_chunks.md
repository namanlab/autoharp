# Extract non-chunks from an Rmd file.

Extracts non-chunks from an Rmd file.

## Usage

``` r
extract_non_chunks(rmd_name, out_name)
```

## Arguments

- rmd_name:

  A character string, the name of the rmd file to get the chunks from.

- out_name:

  An output filename, to dump the text to.

## Value

If out_name is missing, then a character vector is returned. If outfname
is specified, then nothing is returned. The text is written to the file
instead.
