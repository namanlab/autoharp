# Generate a html of thumbnails

Generate a html of thumbnails

## Usage

``` r
generate_thumbnails(out_dir, html_fname, html_title, anonymise = FALSE)
```

## Arguments

- out_dir:

  The directory in which student html files and the figures are kept.

- html_fname:

  The name of the master html file which will contain all thumbnails.
  This file will be created in `out_dir`.

- html_title:

  The title tag of the master html page. This will be displayed on top
  of the output html page.

- anonymise:

  If TRUE, the original filenames will be replaced with inocuous
  numbers. If FALSE, the original filenames will be retained.

## Value

The function returns nothing, but it should create a html page of
thumbnails of all the images that students plotted, along with links to
their individual pages.

## Details

After running
[`render_one`](https://github.com/namanlab/autoharp/reference/render_one.md)
on a set of R/Rmd files in a directory, this function helps to
consolidate them for review.

The output folder contains all the generated html files, images and a
log file. This function will extract the images from each html file and
display them as thumbnails on a new html page, with links to all
individual files.
