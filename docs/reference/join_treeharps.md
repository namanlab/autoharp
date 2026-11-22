# Root a list of trees.

Given a list of trees, this will root them.

## Usage

``` r
join_treeharps(...)
```

## Arguments

- ...:

  A list of Treeharp objects.

## Value

A TreeHarp object

## Details

This function combines TreeHarp objects into a single TreeHarp. The
function will root all of them at a node called "script", which is
neither a function call nor an argument nor a symbol. The BFS ordering
is then updated.

Objects that are not of class TreeHarp will be dropped from the list
before the rooting takes place.
