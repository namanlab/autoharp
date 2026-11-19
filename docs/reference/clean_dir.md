# Removes md Files when no HTML Present

Cleans up the autoharp output directory.

## Usage

``` r
clean_dir(dir_name, verbose = FALSE)
```

## Arguments

- dir_name:

  The directory containing the files to be cleaned.

- verbose:

  If TRUE, then the files and directories being removed will be printed.

## Value

No return value.

## Details

When batch rendering Rmd files, it is inevitable that some files fail.
These files would have their knit.md and utf.md present, but they would
not have a corresponding html file generated.

This function is called for its' side-effect, to remove those lonely md
files.

If this clean-up is not done, when we try to re-run the files (perhaps
with some of the errors fixed), these straggling md files will cause
problems. The most crucial one is that the Rmd files will not be
re-knitted, even though they have been changed.
