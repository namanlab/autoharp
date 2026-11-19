# Generic for Getting Node Types

The generic method definition for getting node types from a TreeHarp
object.

## Usage

``` r
get_node_types(x, ...)

# S4 method for class 'TreeHarp'
get_node_types(x, ...)
```

## Arguments

- x:

  An object of class TreeHarp.

- ...:

  Unused arguments, for now.

## Value

A data frame containing the node types for a TreeHarp object. If the
slot is empty, NA is returned.

## Methods (by class)

- `get_node_types(TreeHarp)`: A getter.

  Allows user to extract the node types of a treeharp object.
