# Calculate Run-time Statistics

This is stand-alone function. It computes the runtime stats without
rendering the md/html/pdf file.

## Usage

``` r
check_runtime(stud_fname, knit_root_dir, return_env = FALSE)
```

## Arguments

- stud_fname:

  The rmd filename of the student.

- knit_root_dir:

  The working directory to use when knitting the file.

- return_env:

  A logical value to indicate if the environment from the rmd file
  should be return. If FALSE, an NA value is returned.

## Value

A list containing the running time in seconds, the memory used by the
final environment in bytes (as a numeric scalar), and the environment
object containing all the generated objects from the rmd file.

## Details

This routine is not used within any other function within the package.
Figures are not cleaned or removed.

## See also

[`render_one`](https://github.com/namanlab/autoharp/reference/render_one.md)
