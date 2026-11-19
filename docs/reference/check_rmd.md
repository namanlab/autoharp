# Check if a File is Rmd

Checks if a file actually is an Rmd file.

## Usage

``` r
check_rmd(fname, verbose = TRUE)
```

## Arguments

- fname:

  A character string. It is the name of the student submission file.

- verbose:

  A logical value that prints messages if a non-rmd file is found.

## Value

The function will return TRUE if all the (3) checks pass, and FALSE
otherwise.

## Details

It runs three checks. First, it checks for the file extension to be Rmd
or rmd or any such variant. Second, it checks for a YAML header at the
beginning of file. Finally, it checks if there is at least one properly
defined R chunk within the file.

## See also

[`get_libraries`](https://github.com/namanlab/autoharp/reference/get_libraries.md)
