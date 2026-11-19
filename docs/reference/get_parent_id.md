# Generic for Getting Parent Node Id.

The generic method definition for getting parent node id.

## Usage

``` r
get_parent_id(x, node_num)

# S4 method for class 'TreeHarp'
get_parent_id(x, node_num)

# S4 method for class 'list'
get_parent_id(x, node_num)
```

## Arguments

- x:

  An object of class TreeHarp or an adjacency list.

- node_num:

  An integer, length 1. This the node whose parent we are after. If
  node_num is equal to 1, then NULL is returned because that should be
  the root node.

## Value

An integer, indicating the parent node.

## Methods (by class)

- `get_parent_id(TreeHarp)`: Obtain parent node id.

  Extracts parent id of a node from a TreeHarp object.

- `get_parent_id(list)`: Obtain parent node id.

  Extracts parent id of a node from an adjacency list object.

## See also

[`get_child_ids`](https://github.com/namanlab/autoharp/reference/get_child_ids.md)
