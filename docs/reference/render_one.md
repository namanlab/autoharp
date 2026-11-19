# Run a single Rmd file through autoharp.

Renders the specified file, and collates run time, static and
correctness checks.

## Usage

``` r
render_one(
  rmd_name,
  out_dir,
  knit_root_dir,
  log_name,
  soln_stuff,
  max_time_per_run = 120,
  permission_to_install = FALSE
)
```

## Arguments

- rmd_name:

  The path to the file to be rendered and checked.

- out_dir:

  The directory to store all the html output, md output, and figures.

- knit_root_dir:

  The working directory while knitting the file.

- log_name:

  A character string, denoting the log file name. It defaults to
  "render_one.log". If this file is already present in the directory,
  this function will append to it.

- soln_stuff:

  This is a list, with components env, test_fname, and tt_list. This
  object is the output of
  [`populate_soln_env`](https://github.com/namanlab/autoharp/reference/populate_soln_env.md).
  Set this to be NA if you wish to skip correctness checks, and only do
  rendering.

- max_time_per_run:

  The maximum time to wait before aborting the rendering of a particular
  file.

- permission_to_install:

  If TRUE, then the function will try to install any packages needed. By
  default, this is FALSE.

## Value

A data frame with one row for each file in the input directory.

## Details

The log file contains a record of the libraries used by the student, and
if any new libraries needed to be installed. The status will be one of
SUCCESS, FAIL or UNKNOWN.

## See also

[`populate_soln_env`](https://github.com/namanlab/autoharp/reference/populate_soln_env.md),
[`check_correctness`](https://github.com/namanlab/autoharp/reference/check_correctness.md)
