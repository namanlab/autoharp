# Get the id and depth of a child node.

From the parent's depth and the last labelled node, we obtain the node
id and depth of a child.

## Usage

``` r
get_next_depth_id(parent_node_id, env_ni)
```

## Arguments

- parent_node_id:

  The id of the parent node we are considering.

- env_ni:

  An environment object, possibly containing a data frame with columns
  id, name, call_status, arg_type and depth.

## Value

A list containing the id and depth of the next node.

## Details

This is for internal use. It may be removed from user-view soon!
