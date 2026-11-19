# Extracts the Packages Used in An Rmd File.

The input filename could correspond to an R script or an Rmd file.

## Usage

``` r
get_libraries(fname)
```

## Arguments

- fname:

  The Rmd filename or R script.

## Value

A character vector containing the packages used within the Rmd document.

## Details

The file is assumed to be either an R script or an Rmd file. If it is
found to be an Rmd file using
[extract_chunks](https://github.com/namanlab/autoharp/reference/extract_chunks.md),
it is purl-ed before libraries are extracted. If it is found to be NOT
an Rmd, it is assumed to be an R script and nothing is done to process
it.

The file is not parsed, so even text files will work with this function.
