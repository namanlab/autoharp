# File lint counter

Count number of lints in one file

## Usage

``` r
count_lints_one(rmd_file, lint_list, lint_labels)
```

## Arguments

- rmd_file:

  The path to the rmd file to check for lints.

- lint_list:

  List of lints to check for.

- lint_labels:

  List of labels to name the vector to return.

## Value

Vector containing the lints.

## Details

The function will count the number of lints in a file. The lints to be
checked can be passed as an argument. Else, the default will be used.
The defaults are as follows: \* T_and_F_symbol_linter \*
line_length_linter \* assignment_linter \* absolute_path_linter \*
pipe_continuation_linter Note that labels would also need to be given if
the non-default lints are chosen.
