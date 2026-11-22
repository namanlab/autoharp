# Generate a dataframe from the log file.

Generate a dataframe from the log file.

## Usage

``` r
log_summary(log_file)
```

## Arguments

- log_file:

  The name of the log file generated from
  [`render_one`](https://github.com/namanlab/autoharp/reference/render_one.md).

## Value

The function returns a dataframe summarising the details in the log
file.

## Details

This provides a table view of the log file, which is updated in a more
natural format by simply concatenating new updates. The output of this
function makes it easier to group entries by filename, time, or status,
or even error message.

The output table does not contain correctness output. It only contains
the columns name, timestamp, status (SUCCESS/FAIL), error message,
number of libraries used and number of libraries installed.

## See also

[`render_one`](https://github.com/namanlab/autoharp/reference/render_one.md)
