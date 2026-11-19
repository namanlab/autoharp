# Extract section text from Rmd

Extract section text from Rmd

## Usage

``` r
extract_section_text(rmd_name, hdr_pat, ignore_case = TRUE)
```

## Arguments

- rmd_name:

  The filename of the Rmd script.

- hdr_pat:

  The regular expression pattern to pick up the section title.

- ignore_case:

  A boolean - whether or not to ignore case when matching for the
  section title.

## Value

Returns a character vector containing all the text written in the
section that begins with the specified pattern.

The pattern should pick up a unique section/sub-section/sub-sub-section.
Otherwise, it will stop and raise an error.

## Details

The text that is picked up begins with the specified section, and ends
with the next string of pound symbols (#)
