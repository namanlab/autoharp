# Lint counter

Count number of lints in one folder

## Usage

``` r
count_lints_all(file_names, lint_list, lint_labels)
```

## Arguments

- file_names:

  The path to the rmd files that need to be checked for lints.

- lint_list:

  List of lints to check for.

- lint_labels:

  List of labels to name the vector to return.

## Value

Dataframe containing the lints.

## Details

The function will count the number of lints in a file. The lints to be
checked can be passed as an argument. Else, the default will be used.
The defaults are as follows:

- T_and_F_symbol_linter

- line_length_linter

- assignment_linter

- absolute_path_linter

- pipe_continuation_linter

Note that labels would also need to be given if the non-default lints
are chosen.
