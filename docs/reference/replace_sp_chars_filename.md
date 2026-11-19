# Replace Special Characters in File Name

Replaces special characters in the name of an R or Rmd script.

## Usage

``` r
replace_sp_chars_filename(dir_name, return_df = TRUE)
```

## Arguments

- dir_name:

  A character string, referring to the directory of Rmd files whose
  names should be replaced.

- return_df:

  A logical value, indicating if the old and new names should be
  returned (in a tibble).

## Value

A tibble containing the old and new names.

## Details

If a filename contains one of the following special characters (ignore
the quotes here): "`[ <>()|\:&;#?*']`", the
[knit](https://rdrr.io/pkg/knitr/man/knit.html) function will replace
them with underscores. Hence the filenames in the autoharp input
directory and the output directory will not match, even allowing for the
change in file extension. This will cause problems when we try to run
[`render_one`](https://github.com/namanlab/autoharp/reference/render_one.md)
again on the same input directory.

This function renames the files in the input directory by replacing all
special characters there.

The NUS LMS (LumiNUS) introduces parenthesized names or numbers in order
to make filenames unique, so this function is necessary for NUS
instructors.
