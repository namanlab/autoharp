# Forestharp helpers

Example of functions that can be *directly used on TreeHarp objects
individually,* and on forestharp objects via
[`fapply`](https://github.com/namanlab/autoharp/reference/fapply.md).

## Usage

``` r
count_self_fn(th)

count_lam_fn(th)

count_fn_call(th, pattern, pkg_name)

extract_fn_call(th, pattern, pkg_name)

extract_formal_args(th, fn_name)

extract_assigned_objects(th)

extract_actual_args(th, include_assigned_obj = TRUE)

count_num_lines_for_loop(th)

detect_growing(th, count = FALSE, within_for = FALSE)

detect_for_in_fn_def(th, fn_name)

count_fn_in_fn(th, fn_name, sub_fn)

detect_fn_call_in_for(th, fn_name)

extract_self_fn(th)

detect_fn_arg(th, fn_name, arg)

detect_nested_for(th)
```

## Arguments

- th:

  A TreeHarp object.

- pattern:

  A regular expression to pick up function names.

- pkg_name:

  The name of a package to match functions with. This should be an exact
  match for the package name. The package should be attached for this to
  work. In order to avoid picking up duplicate names, for instance
  `tolower` is a function in base R and in ggplot2, run
  [`get_libraries`](https://github.com/namanlab/autoharp/reference/get_libraries.md)
  on the file as well, and match against it.

- fn_name:

  Function name, as a character string

- include_assigned_obj:

  A Boolean flag. If TRUE (default), it also returns the name of the
  assigned object. If FALSE, it drops the name of the assigned object.
  The FALSE flag is useful for when checking if the we wish to match the
  assigned object from a previous call with the actual argument in this
  call.

- count:

  For `detect_growing`, this is a logical value that indicates if the
  number of "grow" expressions should be counted and returned, or if
  just a logical value should be returned.

- within_for:

  If TRUE, only expresssions within a for loop are included.

- sub_fn:

  (For count_fn_in_fn), the function to count (to look for within
  fn_name).

- arg:

  The argument to check for within fn_name (as a character string).

## Value

On their own, each of these functions should return a scalar or a
1-dimensional array. When called with
[`fapply`](https://github.com/namanlab/autoharp/reference/fapply.md),
the scalar numerical values can be combined (by taking the sum, any
other provided combiner function).

The ultimate idea is that fapply should return a single feature for each
rmd file that it is called upon.

## Details

These are examples of functions that be called on a list of TreeHarp
objects, which we refer to as a forestharp object. Such objects are not
formally defined yet, but can be created using
[`rmd_to_forestharp`](https://github.com/namanlab/autoharp/reference/rmd_to_forestharp.md)
or using
[`join_treeharps`](https://github.com/namanlab/autoharp/reference/join_treeharps.md).

## Functions

- `count_self_fn()`: Counts the number of self-defined functions.

  This helper counts the number of self-defined functions. It excludes
  lambda functions. It returns an integer scalar.

  As long as the function `function` was called and assigned, it will be
  counted.

- `count_lam_fn()`: Counts the number of anonymous functions.

  Counts the number of anonymous functions, typically used in sapply,
  etc. It returns an integer scalar. As long as the function `function`
  was called but *not* assigned, it will be counted here.

- `count_fn_call()`: Counts the number of function calls that match a
  pattern.

  This helper counts the number of function calls that match a pattern.
  It returns a count, i.e. an integer vector of length 1.

  If `pkg_name` is provided instead of `pattern`, then this function
  counts the number of function calls from that package.

- `extract_fn_call()`: Extracts function calls as a string.

  Extracts the function calls that match a pattern. It returns a
  character vector. Remember to set `combine = FALSE` when calling
  [`fapply`](https://github.com/namanlab/autoharp/reference/fapply.md)
  with it.

- `extract_formal_args()`: Extracts function formal arguments called.

  Extracts the function *formal* arguments from functions with a given
  name. The name must match the function name exactly. This returns a
  character vector or NULL, if no formal arguments are used.

- `extract_assigned_objects()`: Extracts names of assigned objects

  Extracts the names of assigned objects. This was written to assist in
  detecting missed opportunities to use the pipe operator.

- `extract_actual_args()`: Extracts actual argument names

  Extracts the actual arguments from an expression, not the formal
  arguments. It only returns syntactic literals. It should be improved
  to return the actual arguments for a specified function so that
  something similar to `extract_assigned_objects` could be returned.

- `count_num_lines_for_loop()`: Counts number of lines in a for loop

  Extracts the actual arguments from an expression, not the formal
  First, this function checks if the expression contains a for loop. If
  it does, the innermost `for` loop is identified. Finally, the number
  of expressions below it is counted and returned.

- `detect_growing()`: Detects if a vector is being grown.

  It detects if there is an expression of form: x \<- c(x, new_val).
  This is generally bad programming practice

- `detect_for_in_fn_def()`: Detects if a for loop is present within a
  function

  It detects if a for loop is present within a function definition.

- `count_fn_in_fn()`: Count use of a function within another.

  It counts the number of times a function is used within another.

- `detect_fn_call_in_for()`: Detect for loop to call a function

  Checks if a function has been called within a for loop.

- `extract_self_fn()`: Extract names of functions defined by user.

  Extracts names of user-defined functions. They may not all look nice,
  because sum functions may be anonymous functions. This function needs
  to be improved.

- `detect_fn_arg()`: Was a function called with a particular argument?

  Checks if a function was called with a particular argument, which
  could be the formal or actual one. The immediate child of the function
  call node is checked.

- `detect_nested_for()`: Was a nested "for" loop called anywhere within
  the code?

  Checks if a nested for-loop was called anywhere within the code. This
  returns a logical scalar for each TreeHarp object given.

## Examples

``` r
# Dummy trees
th1 <- TreeHarp(quote(X <- rnorm(10, mean=0.9, sd=4)), TRUE)
th2 <- TreeHarp(quote(Y <- rbeta(10, shape1=3, shape2=5)), TRUE)
th3 <- TreeHarp(quote(fn1 <- function(x) x + 2), TRUE)
th4 <- TreeHarp(quote(df1 <- mutate(df1, new_col=2*old_col)), TRUE)

# Run helpers
count_self_fn(th3)
#> [1] 1
count_fn_call(th4, pkg_name="dplyr")
#> [1] 1
count_fn_call(th1, pattern="^r.*")
#> [1] 1
```
