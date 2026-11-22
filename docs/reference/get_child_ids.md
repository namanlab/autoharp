# Generic for Getting Child Node Ids

The generic method definition for getting child node ids.

## Usage

``` r
get_child_ids(x, node_num)

# S4 method for class 'TreeHarp'
get_child_ids(x, node_num)

# S4 method for class 'list'
get_child_ids(x, node_num)
```

## Arguments

- x:

  An object of class TreeHarp.

- node_num:

  An integer, length 1. This the node whose children we are after. If
  the specified node is a leaf, the NULL is returned.

## Value

An integer vector, indicating the children node ids.

## Methods (by class)

- `get_child_ids(TreeHarp)`: Obtain child nodes.

  Allows user to extract the child nodes from a specified node from
  TreeHarp object.

- `get_child_ids(list)`: Obtain child nodes.

  Allows user to extract the child nodes from a specified node from an
  adjacency list.

## See also

[`get_parent_id`](https://github.com/namanlab/autoharp/reference/get_parent_id.md)
